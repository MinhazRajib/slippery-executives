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
    background_only ~config:{ Synthetic_market.Config.seed = 1 } flat_day
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
    background_only ~config:{ Synthetic_market.Config.seed = 7 } trending_day
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
            ~config:{ Synthetic_market.Config.seed }
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
      (Synthetic_market.create { seed = 1 })
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
      ~engine:(Synthetic_market.engine { seed = 1 })
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
