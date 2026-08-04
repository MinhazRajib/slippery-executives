open! Core
open! Execlab_types
open! Execlab_market
open! Execlab_execution
open! Execlab_simulation
open Execlab_exchange

let time_at_minute i =
  Option.value_exn
    (Time_ns.Ofday.add
       (Time_ns.Ofday.of_string "09:30:00")
       (Time_ns.Span.of_int_min i))
;;

let bar ~minute ~open_ ~high ~low ~close ~volume =
  Or_error.ok_exn
    (Market_bar.create
       ~time:(time_at_minute minute)
       ~open_:(Price.of_int_cents open_)
       ~high:(Price.of_int_cents high)
       ~low:(Price.of_int_cents low)
       ~close:(Price.of_int_cents close)
       ~volume:(Size.of_int volume))
;;

let day bars =
  Or_error.ok_exn
    (Trading_day.create
       ~symbol:(Symbol.of_string "NVDA")
       ~date:(Date.of_string "2026-07-09")
       ~bars)
;;

let flat_day =
  day
    (List.init 390 ~f:(fun i ->
       bar
         ~minute:i
         ~open_:15000
         ~high:15000
         ~low:15000
         ~close:15000
         ~volume:42000))
;;

(* A steady climb: each bar opens where the last closed and gains 2c, with a
   little range around it. *)
let trending_day =
  day
    (List.init 390 ~f:(fun i ->
       let open_ = 15000 + (2 * i) in
       bar
         ~minute:i
         ~open_
         ~high:(open_ + 4)
         ~low:(open_ - 2)
         ~close:(open_ + 2)
         ~volume:42000))
;;

(* Runs the background tape over a whole day with no client orders. *)
let background_only ~config day =
  let market =
    List.fold
      day.Trading_day.bars
      ~init:(Synthetic_market.create config)
      ~f:(fun market bar ->
        let market, (fills : Fill.t list) =
          Synthetic_market.on_bar_advance market ~bar ~resting_orders:[]
        in
        assert (List.is_empty fills);
        market)
  in
  Synthetic_market.For_testing.finish_day market
;;

let%expect_test "zero clients, flat day: the tape prints at the historical \
                 price"
  =
  (* Makers ladder symmetrically around 150.00 with a 1c base half-spread and
     noise hits both sides, so the tape's VWAP must sit within the tightest
     quote of the fundamental. *)
  let market =
    background_only
      ~config:{ Synthetic_market.Config.default with seed = 1 }
      flat_day
  in
  let vwap =
    Option.value_exn (Synthetic_market.For_testing.sim_vwap market)
  in
  printf
    "|sim vwap - 150.00| <= 0.01: %b\n"
    Float.(abs (vwap -. 150.) <= 0.01);
  [%expect {| |sim vwap - 150.00| <= 0.01: true |}]
;;

let%expect_test "zero clients, trending day: sim vwap tracks the historical \
                 vwap"
  =
  let market =
    background_only
      ~config:{ Synthetic_market.Config.default with seed = 7 }
      trending_day
  in
  let sim =
    Option.value_exn (Synthetic_market.For_testing.sim_vwap market)
  in
  let historical = Day_stats.vwap trending_day in
  let gap_bps = Float.abs ((sim -. historical) /. historical) *. 10_000. in
  printf "gap under 5 bps: %b\n" Float.(gap_bps < 5.);
  [%expect {| gap under 5 bps: true |}]
;;

let%expect_test "same seed, same tape; different seed, different tape" =
  let vwap seed =
    Option.value_exn
      (Synthetic_market.For_testing.sim_vwap
         (background_only
            ~config:{ Synthetic_market.Config.default with seed }
            trending_day))
  in
  printf "reproducible: %b\n" (Float.equal (vwap 3) (vwap 3));
  printf "seed-sensitive: %b\n" (not (Float.equal (vwap 3) (vwap 4)));
  [%expect {|
    reproducible: true
    seed-sensitive: true
    |}]
;;

let child ~quantity =
  let request =
    Or_error.ok_exn
      (Child_order.Request.create
         ~symbol:(Symbol.of_string "NVDA")
         ~side:Side.Buy
         ~quantity:(Size.of_int quantity)
         ~order_type:Order_type.Market
         ~time_in_force:Time_in_force.IOC)
  in
  Child_order.create
    ~request
    ~id:(Order_id.For_testing.of_int 1)
    ~submitted_at:(time_at_minute 1)
;;

let%expect_test "a market order walks the ladder: worse prices at each \
                 level, and depth exhaustion caps the fill"
  =
  (* Flat day, so the ladder quotes around 150.00 with a 1c half-spread: asks
     at 150.01 / 150.02 / 150.04 with roughly 2% / 4% / 6% of the
     42,000-share bar behind them (jittered by the seeded rng). A
     10,000-share market buy must fill in exactly three ever-worse steps; the
     total displayed is under 6,000, so an outsized order gets only what the
     book holds. *)
  let market, (_ : Fill.t list) =
    Synthetic_market.on_bar_advance
      (Synthetic_market.create
         { Synthetic_market.Config.default with seed = 1 })
      ~bar:(List.hd_exn flat_day.Trading_day.bars)
      ~resting_orders:[]
  in
  let (_ : Synthetic_market.t), fills =
    Synthetic_market.on_child_order market (child ~quantity:10_000)
  in
  List.iter fills ~f:(fun fill ->
    printf
      "%d @ %s (%s)\n"
      (Size.to_int fill.size)
      (Price.to_string_dollar fill.price)
      (match fill.liquidity with Taker -> "taker" | Maker -> "maker"));
  let total = List.sum (module Int) fills ~f:(fun f -> Size.to_int f.size) in
  printf "filled %d of 10,000: partial %b\n" total (total < 10_000);
  [%expect
    {|
    909 @ $150.01 (taker)
    1800 @ $150.02 (taker)
    2506 @ $150.04 (taker)
    filled 5215 of 10,000: partial true
    |}]
;;

let%expect_test "the driver runs a whole session in the synthetic market \
                 and completes"
  =
  let instructions =
    [ Or_error.ok_exn
        (Alpha_instruction.create
           ~arrival_time:(Time_ns.Ofday.of_string "10:05:00")
           ~symbol:(Symbol.of_string "NVDA")
           ~side:Side.Buy
           ~quantity:(Size.of_int 1000)
           ~deadline:(Time_ns.Ofday.of_string "11:00:00"))
    ]
  in
  let result =
    Driver.run
      ~day:flat_day
      ~instructions
      ~algorithm:(module Execlab_execution.Twap)
      ~engine:
        (Synthetic_market.engine
           { Synthetic_market.Config.default with seed = 1 })
      ()
  in
  List.iter
    (Execlab_execution.Order_manager.parents result.manager)
    ~f:(fun parent ->
      printf
        "parent: %s filled=%d\n"
        (Sexp.to_string
           [%sexp (parent.status : Execlab_execution.Parent_order.Status.t)])
        (Size.to_int parent.filled));
  let worst =
    List.fold result.fills ~init:0 ~f:(fun acc fill ->
      Int.max acc (Price.to_int_cents fill.Fill.price))
  in
  printf "every fill within the ladder: %b\n" (worst <= 15004);
  [%expect
    {|
    parent: Completed filled=1000
    every fill within the ladder: true
    |}]
;;

let%expect_test "a penny-priced bar drops ladder rungs below one cent \
                 instead of piling them onto it"
  =
  (* Open 3c with a 1c base half-spread: the bid ladder computes 2c, 1c, -1c;
     the -1c rung must vanish, not clamp onto the 1c level as phantom depth.
     Bids stay strictly below asks. *)
  let market, (_ : Fill.t list) =
    Synthetic_market.on_bar_advance
      (Synthetic_market.create
         { Synthetic_market.Config.default with seed = 1 })
      ~bar:(bar ~minute:0 ~open_:3 ~high:9 ~low:2 ~close:8 ~volume:50_000)
      ~resting_orders:[]
  in
  let book = Synthetic_market.For_testing.book market in
  let best side = Book.best book ~side in
  printf
    "bid < ask: %b\n"
    (match best Side.Buy, best Side.Sell with
     | Some bid, Some ask -> Price.( < ) bid ask
     | (Some (_ : Price.t) | None), (Some (_ : Price.t) | None) -> false);
  print_s [%sexp (book : Book.t)];
  [%expect
    {|
    bid < ask: true
    ((bids ((2 1217) (1 1968))) (asks ((4 1082) (5 2143) (7 2983))))
    |}]
;;

let touch market =
  let book = Synthetic_market.For_testing.book market in
  ( Option.value_exn (Book.best book ~side:Side.Buy)
  , Option.value_exn (Book.best book ~side:Side.Sell) )
;;

let%expect_test "spread calibration: the touch tracks the bar's own range \
                 across the bundled names"
  =
  (* The premise has to come from the data, not from a bar chosen to flatter
     it. Median minute ranges across data/ run 7c (NFLX), 46c (TSLA) and 78c
     (META), and the catalog's own median is 23c. A rung of 0.42 *
     sqrt(range) quotes those at 1c, 3c and 4c either side of the open, and
     the catalog median at exactly the bar model's 2c default: a typical
     minute costs what Engine A says it costs, quiet names quote tighter and
     violent ones wider, and the spread grows far more slowly than the range
     — as real spreads do. *)
  let touch_for ~range_cents =
    let market, (_ : Fill.t list) =
      Synthetic_market.on_bar_advance
        (Synthetic_market.create Synthetic_market.Config.default)
        ~bar:
          (bar
             ~minute:0
             ~open_:15000
             ~high:(15000 + (range_cents / 2))
             ~low:(15000 - (range_cents / 2))
             ~close:15000
             ~volume:62_429)
        ~resting_orders:[]
    in
    let bid, ask = touch market in
    printf
      "range %2dc -> touch %s / %s (half-spread %dc)\n"
      range_cents
      (Price.to_string_dollar bid)
      (Price.to_string_dollar ask)
      ((Price.to_int_cents ask - Price.to_int_cents bid) / 2)
  in
  touch_for ~range_cents:7;
  touch_for ~range_cents:46;
  touch_for ~range_cents:78;
  [%expect
    {|
    range  7c -> touch $149.99 / $150.01 (half-spread 1c)
    range 46c -> touch $149.97 / $150.03 (half-spread 3c)
    range 78c -> touch $149.96 / $150.04 (half-spread 4c)
    |}]
;;

let%expect_test "permanent impact: the makers remember being run over, and \
                 forget slowly"
  =
  (* The seeded 10,000-share buy takes 5,215 shares (see the ladder test
     above), so pressure starts at +5,215 and is scaled by 0.6 each bar
     before the makers requote. The centre shifts by 15c * sqrt(pressure /
     bar volume): 4c, then 3c, then 2c — a dent that fades rather than one
     that vanishes at the next bar or never heals. *)
  let market, (_ : Fill.t list) =
    Synthetic_market.on_bar_advance
      (Synthetic_market.create Synthetic_market.Config.default)
      ~bar:(List.hd_exn flat_day.Trading_day.bars)
      ~resting_orders:[]
  in
  let market, (_ : Fill.t list) =
    Synthetic_market.on_child_order market (child ~quantity:10_000)
  in
  let market =
    List.fold
      (List.take (List.tl_exn flat_day.Trading_day.bars) 3)
      ~init:market
      ~f:(fun market bar ->
        let market, (_ : Fill.t list) =
          Synthetic_market.on_bar_advance market ~bar ~resting_orders:[]
        in
        let (_ : Price.t), ask = touch market in
        printf "best ask %s\n" (Price.to_string_dollar ask);
        market)
  in
  ignore (market : Synthetic_market.t);
  [%expect
    {|
    best ask $150.05
    best ask $150.04
    best ask $150.03
    |}]
;;

let limit_child ~id ~quantity ~price_cents =
  let request =
    Or_error.ok_exn
      (Child_order.Request.create
         ~symbol:(Symbol.of_string "NVDA")
         ~side:Side.Buy
         ~quantity:(Size.of_int quantity)
         ~order_type:(Order_type.Limit (Price.of_int_cents price_cents))
         ~time_in_force:Time_in_force.Day)
  in
  Child_order.create
    ~request
    ~id:(Order_id.For_testing.of_int id)
    ~submitted_at:(time_at_minute 1)
;;

let%expect_test "a resting limit waits its turn in the queue" =
  (* Two client buys rest through the same flow, each wanting far more than
     the bar will give them. One improves on the best bid by posting 150.00,
     where no agent is showing; the other joins the crowd at 149.99, behind
     the 1,022 shares displayed there.

     A bar trades a finite amount, and price priority decides who gets it. In
     the first bar the improver takes the entire servable sell flow — 9,785
     shares — and nothing reaches the joiner at all. In the second the
     improver finishes its 20,000 with 10,215 more, and of the 4,720 shares
     of flow left over the joiner must first serve the 1,022 ahead of it,
     filling 3,698. Both are paid the spread rather than paying it: maker
     fills at their own limit. *)
  let market, (_ : Fill.t list) =
    Synthetic_market.on_bar_advance
      (Synthetic_market.create Synthetic_market.Config.default)
      ~bar:(List.hd_exn flat_day.Trading_day.bars)
      ~resting_orders:[]
  in
  let ahead =
    Book.size_at
      (Synthetic_market.For_testing.book market)
      ~side:Side.Buy
      ~price:(Price.of_int_cents 14999)
  in
  printf "agent size displayed at 149.99: %d\n" ahead;
  let improver = limit_child ~id:1 ~quantity:20_000 ~price_cents:15000 in
  let joiner = limit_child ~id:2 ~quantity:20_000 ~price_cents:14999 in
  let market, (_ : Fill.t list) =
    Synthetic_market.on_child_order market improver
  in
  let market, (_ : Fill.t list) =
    Synthetic_market.on_child_order market joiner
  in
  (* Fills are applied to the children between bars, exactly as the driver
     does, so nobody trades more than they asked for. *)
  let (_ : Synthetic_market.t), filled =
    List.fold
      (List.take (List.tl_exn flat_day.Trading_day.bars) 2)
      ~init:(market, [ improver; joiner ])
      ~f:(fun (market, resting_orders) bar ->
        let market, fills =
          Synthetic_market.on_bar_advance market ~bar ~resting_orders
        in
        List.iter fills ~f:(fun (fill : Fill.t) ->
          printf
            "  filled %d @ %s (%s)\n"
            (Size.to_int fill.size)
            (Price.to_string_dollar fill.price)
            (match fill.liquidity with Taker -> "taker" | Maker -> "maker"));
        let resting_orders =
          List.map resting_orders ~f:(fun (child : Child_order.t) ->
            List.fold fills ~init:child ~f:(fun child (fill : Fill.t) ->
              if Order_id.equal fill.order_id child.id
              then Child_order.apply_fill_exn child ~quantity:fill.size
              else child))
        in
        market, resting_orders)
  in
  List.iter filled ~f:(fun (child : Child_order.t) ->
    printf
      "order %s total filled: %d\n"
      (Sexp.to_string [%sexp (child.id : Order_id.t)])
      (20_000 - Size.to_int child.remaining));
  [%expect
    {|
    agent size displayed at 149.99: 1022
      filled 9785 @ $150.00 (maker)
      filled 10215 @ $150.00 (maker)
      filled 3698 @ $149.99 (maker)
    order 1 total filled: 20000
    order 2 total filled: 3698
    |}]
;;

let%expect_test "a resting limit priced through the touch crosses instead \
                 of collecting a maker fill"
  =
  (* A buy limit above the offer is not resting, whatever the driver calls
     it: the makers would have hit it the moment they quoted. It must take at
     the book's price and be charged as a taker, not sit at the front of a
     queue and collect the spread at its own limit. *)
  let market, (_ : Fill.t list) =
    Synthetic_market.on_bar_advance
      (Synthetic_market.create Synthetic_market.Config.default)
      ~bar:(List.hd_exn flat_day.Trading_day.bars)
      ~resting_orders:[]
  in
  let through = limit_child ~id:1 ~quantity:400 ~price_cents:15_010 in
  let (_ : Synthetic_market.t), fills =
    Synthetic_market.on_bar_advance
      market
      ~bar:(List.nth_exn flat_day.Trading_day.bars 1)
      ~resting_orders:[ through ]
  in
  List.iter fills ~f:(fun (fill : Fill.t) ->
    printf
      "filled %d @ %s (%s)\n"
      (Size.to_int fill.size)
      (Price.to_string_dollar fill.price)
      (match fill.liquidity with Taker -> "taker" | Maker -> "maker"));
  [%expect {| filled 400 @ $150.01 (taker) |}]
;;

let%expect_test "the zero-client calibration holds on a wide-range day too" =
  (* The flat and trending fixtures both floor to a one-cent rung, so they
     cannot tell one spread model from another. This day's minutes swing 60c
     — a four-cent rung — and the background tape must still track the
     historical VWAP, which is what says the ladders are centred rather than
     merely narrow. *)
  let wide =
    let bar_at minute =
      let open_ = 15_000 + (minute % 7 * 10) in
      Or_error.ok_exn
        (Market_bar.create
           ~time:(time_at_minute minute)
           ~open_:(Price.of_int_cents open_)
           ~high:(Price.of_int_cents (open_ + 30))
           ~low:(Price.of_int_cents (open_ - 30))
           ~close:(Price.of_int_cents (open_ + 5))
           ~volume:(Size.of_int 50_000))
    in
    Or_error.ok_exn
      (Trading_day.create
         ~symbol:(Symbol.of_string "NVDA")
         ~date:(Date.of_string "2026-07-09")
         ~bars:(List.init 390 ~f:bar_at))
  in
  let market =
    background_only ~config:Synthetic_market.Config.default wide
  in
  let sim =
    Option.value_exn (Synthetic_market.For_testing.sim_vwap market)
  in
  let historical = Day_stats.vwap wide in
  printf "sim %.4f vs historical %.4f\n" sim historical;
  printf
    "within 5bps: %b\n"
    (Float.( < ) (Float.abs (sim -. historical) /. historical) 0.0005);
  [%expect
    {|
    sim 150.3646 vs historical 150.3154
    within 5bps: true
    |}]
;;
