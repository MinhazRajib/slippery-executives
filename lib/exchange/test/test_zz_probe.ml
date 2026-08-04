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

let flat_bar i =
  bar
    ~minute:i
    ~open_:15000
    ~high:15000
    ~low:15000
    ~close:15000
    ~volume:42000
;;

let flat_day = day (List.init 390 ~f:flat_bar)

let market_child ~id ~quantity ~side =
  let request =
    Or_error.ok_exn
      (Child_order.Request.create
         ~symbol:(Symbol.of_string "NVDA")
         ~side
         ~quantity:(Size.of_int quantity)
         ~order_type:Order_type.Market
         ~time_in_force:Time_in_force.IOC)
  in
  Child_order.create
    ~request
    ~id:(Order_id.For_testing.of_int id)
    ~submitted_at:(time_at_minute 1)
;;

let limit_child ~id ~quantity ~price_cents ~side =
  let request =
    Or_error.ok_exn
      (Child_order.Request.create
         ~symbol:(Symbol.of_string "NVDA")
         ~side
         ~quantity:(Size.of_int quantity)
         ~order_type:(Order_type.Limit (Price.of_int_cents price_cents))
         ~time_in_force:Time_in_force.Day)
  in
  Child_order.create
    ~request
    ~id:(Order_id.For_testing.of_int id)
    ~submitted_at:(time_at_minute 1)
;;

let touch market =
  let book = Synthetic_market.For_testing.book market in
  Book.best book ~side:Side.Buy, Book.best book ~side:Side.Sell
;;

let show_touch market =
  let bid, ask = touch market in
  let s = function None -> "-" | Some p -> Price.to_string_dollar p in
  printf "  touch %s / %s\n" (s bid) (s ask)
;;

(* PROBE 1: several client orders resting at the same price each fill from
   the SAME bar flow. *)
let%expect_test "probe: resting orders share (do not consume) the flow" =
  let market, (_ : Fill.t list) =
    Synthetic_market.on_bar_advance
      (Synthetic_market.create Synthetic_market.Config.default)
      ~bar:(List.hd_exn flat_day.Trading_day.bars)
      ~resting_orders:[]
  in
  let orders =
    List.init 5 ~f:(fun i ->
      limit_child
        ~id:(i + 1)
        ~quantity:50_000
        ~price_cents:15000
        ~side:Side.Buy)
  in
  let market =
    List.fold orders ~init:market ~f:(fun market child ->
      let market, (_ : Fill.t list) =
        Synthetic_market.on_child_order market child
      in
      market)
  in
  let (_ : Synthetic_market.t), fills =
    Synthetic_market.on_bar_advance
      market
      ~bar:(List.nth_exn flat_day.Trading_day.bars 1)
      ~resting_orders:orders
  in
  printf "bar volume: 42000\n";
  List.iter fills ~f:(fun (fill : Fill.t) ->
    printf
      "  order %s filled %d @ %s\n"
      (Sexp.to_string [%sexp (fill.order_id : Order_id.t)])
      (Size.to_int fill.size)
      (Price.to_string_dollar fill.price));
  printf
    "total client passive shares in one bar: %d\n"
    (List.sum (module Int) fills ~f:(fun f -> Size.to_int f.size));
  [%expect
    {|
    bar volume: 42000
      order 1 filled 9785 @ $150.00
      order 2 filled 9785 @ $150.00
      order 3 filled 9785 @ $150.00
      order 4 filled 9785 @ $150.00
      order 5 filled 9785 @ $150.00
    total client passive shares in one bar: 48925
    |}]
;;

(* PROBE 2: sustained aggression - fixed point of the pressure shift. *)
let%expect_test "probe: sustained buying, pressure fixed point" =
  let market =
    ref
      (fst
         (Synthetic_market.on_bar_advance
            (Synthetic_market.create Synthetic_market.Config.default)
            ~bar:(List.hd_exn flat_day.Trading_day.bars)
            ~resting_orders:[]))
  in
  List.iteri
    (List.take (List.tl_exn flat_day.Trading_day.bars) 25)
    ~f:(fun i bar ->
      (* a very aggressive client: wants far more than the ladder holds *)
      let m, fills =
        Synthetic_market.on_child_order
          !market
          (market_child ~id:(i + 1) ~quantity:1_000_000 ~side:Side.Buy)
      in
      let took =
        List.sum (module Int) fills ~f:(fun f -> Size.to_int f.size)
      in
      let m, (_ : Fill.t list) =
        Synthetic_market.on_bar_advance m ~bar ~resting_orders:[]
      in
      market := m;
      if i % 5 = 0 || i > 20
      then (
        printf "bar %d took %d\n" i took;
        show_touch !market));
  [%expect
    {|
    bar 0 took 5215
      touch $150.03 / $150.05
    bar 5 took 4404
      touch $150.05 / $150.07
    bar 10 took 4404
      touch $150.05 / $150.07
    bar 15 took 4949
      touch $150.06 / $150.08
    bar 20 took 5554
      touch $150.06 / $150.08
    bar 21 took 5725
      touch $150.06 / $150.08
    bar 22 took 4584
      touch $150.05 / $150.07
    bar 23 took 4615
      touch $150.05 / $150.07
    bar 24 took 4841
      touch $150.05 / $150.07
    |}]
;;

(* PROBE 3: pressure built on a busy bar, then a quiet bar. *)
let%expect_test "probe: pressure divided by a collapsed bar volume" =
  let busy =
    bar
      ~minute:0
      ~open_:15000
      ~high:15030
      ~low:14970
      ~close:15000
      ~volume:200_000
  in
  let quiet =
    bar
      ~minute:1
      ~open_:15000
      ~high:15002
      ~low:14998
      ~close:15000
      ~volume:300
  in
  let market, (_ : Fill.t list) =
    Synthetic_market.on_bar_advance
      (Synthetic_market.create Synthetic_market.Config.default)
      ~bar:busy
      ~resting_orders:[]
  in
  let market, fills =
    Synthetic_market.on_child_order
      market
      (market_child ~id:1 ~quantity:1_000_000 ~side:Side.Buy)
  in
  printf
    "took %d on a 200k bar\n"
    (List.sum (module Int) fills ~f:(fun f -> Size.to_int f.size));
  let market, (_ : Fill.t list) =
    Synthetic_market.on_bar_advance market ~bar:quiet ~resting_orders:[]
  in
  show_touch market;
  [%expect
    {|
    took 24836 on a 200k bar
      touch $151.05 / $151.07
    |}]
;;

(* PROBE 4: zero-volume bar. *)
let%expect_test "probe: zero volume bar" =
  let z =
    bar ~minute:0 ~open_:15000 ~high:15000 ~low:15000 ~close:15000 ~volume:0
  in
  let market, (_ : Fill.t list) =
    Synthetic_market.on_bar_advance
      (Synthetic_market.create Synthetic_market.Config.default)
      ~bar:z
      ~resting_orders:[]
  in
  printf !"book: %{sexp:Book.t}\n" (Synthetic_market.For_testing.book market);
  [%expect {| book: ((bids ()) (asks ())) |}]
;;

(* PROBE 5: a resting buy limit above the refreshed best ask. *)
let%expect_test "probe: resting limit that crosses the fresh book" =
  let market, (_ : Fill.t list) =
    Synthetic_market.on_bar_advance
      (Synthetic_market.create Synthetic_market.Config.default)
      ~bar:(List.hd_exn flat_day.Trading_day.bars)
      ~resting_orders:[]
  in
  show_touch market;
  (* limit way above the offers: takes the whole ladder, rests the rest *)
  let child =
    limit_child ~id:1 ~quantity:200_000 ~price_cents:15050 ~side:Side.Buy
  in
  let market, fills = Synthetic_market.on_child_order market child in
  printf
    "took %d immediately\n"
    (List.sum (module Int) fills ~f:(fun f -> Size.to_int f.size));
  let child =
    List.fold fills ~init:child ~f:(fun child (fill : Fill.t) ->
      Child_order.apply_fill_exn child ~quantity:fill.size)
  in
  let market, fills =
    Synthetic_market.on_bar_advance
      market
      ~bar:(List.nth_exn flat_day.Trading_day.bars 1)
      ~resting_orders:[ child ]
  in
  show_touch market;
  List.iter fills ~f:(fun (fill : Fill.t) ->
    printf
      "  resting fill %d @ %s (%s)\n"
      (Size.to_int fill.size)
      (Price.to_string_dollar fill.price)
      (match fill.liquidity with Taker -> "taker" | Maker -> "maker"));
  [%expect
    {|
      touch $149.99 / $150.01
    took 5215 immediately
      touch $150.03 / $150.05
      resting fill 9785 @ $150.50 (maker)
    |}]
;;

(* PROBE 6: zero-client tape is bit-identical to main's structure? just show
   the vwap and print volume for the record. *)
let%expect_test "probe: zero-client tape stats" =
  let market =
    List.fold
      flat_day.Trading_day.bars
      ~init:(Synthetic_market.create Synthetic_market.Config.default)
      ~f:(fun market bar ->
        let market, (_ : Fill.t list) =
          Synthetic_market.on_bar_advance market ~bar ~resting_orders:[]
        in
        market)
  in
  let market = Synthetic_market.For_testing.finish_day market in
  printf
    "vwap %.6f\n"
    (Option.value_exn (Synthetic_market.For_testing.sim_vwap market));
  [%expect {| vwap 150.007821 |}]
;;
