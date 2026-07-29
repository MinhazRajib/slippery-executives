open! Core
open! Execlab_types
open! Execlab_market
open! Execlab_execution
open Execlab_simulation

(* Fixture bar: open 150.20, high 150.45, low 150.10, close 150.38, volume
   42,000. Config: half-spread 2c, participation cap 10% (budget 4,200),
   impact 10c at 100% participation (square-root scaled). *)

let config =
  { Fill_model.Config.half_spread = Price.of_int_cents 2
  ; max_participation = 0.1
  ; impact_coefficient = Price.of_int_cents 10
  }
;;

let bar ?(volume = 42000) ?(low = 15010) ?(high = 15045) () =
  Or_error.ok_exn
    (Market_bar.create
       ~time:(Time_ns.Ofday.of_string "10:05:00")
       ~open_:(Price.of_int_cents 15020)
       ~high:(Price.of_int_cents high)
       ~low:(Price.of_int_cents low)
       ~close:(Price.of_int_cents 15038)
       ~volume:(Size.of_int volume))
;;

let child ~id ~side ~quantity ~order_type ~time_in_force =
  let request =
    Or_error.ok_exn
      (Child_order.Request.create
         ~symbol:(Symbol.of_string "NVDA")
         ~side
         ~quantity:(Size.of_int quantity)
         ~order_type
         ~time_in_force)
  in
  Child_order.create
    ~request
    ~id:(Order_id.For_testing.of_int id)
    ~submitted_at:(Time_ns.Ofday.of_string "10:05:00")
;;

let market_child ~id ~side ~quantity =
  child
    ~id
    ~side
    ~quantity
    ~order_type:Order_type.Market
    ~time_in_force:Time_in_force.IOC
;;

let limit_child ~id ~side ~quantity ~limit =
  child
    ~id
    ~side
    ~quantity
    ~order_type:(Order_type.Limit (Price.of_int_cents limit))
    ~time_in_force:Time_in_force.Day
;;

(* An engine that has seen the fixture bar and holds no resting orders. *)
let fresh ?volume ?low ?high () =
  fst
    (Fill_model.on_bar_advance
       (Fill_model.create config)
       ~bar:(bar ?volume ?low ?high ())
       ~resting_orders:[])
;;

let print_fills fills = print_s [%sexp (fills : Fill.t list)]

let%expect_test "a market buy is capped by participation, pays spread and \
                 impact"
  =
  (* budget = 10% of 42,000 = 4,200. ask = open + 2c = 150.22. impact = 10c *
     sqrt(4200/42000) = 3.16c -> 3c. Fill 4,200 @ 150.25, Taker. *)
  let (_ : Fill_model.t), fills =
    Fill_model.on_child_order
      (fresh ())
      (market_child ~id:1 ~side:Side.Buy ~quantity:5000)
  in
  print_fills fills;
  [%expect
    {|
    (((fill_id 1) (symbol NVDA) (price 15025) (size 4200) (order_id 1) (side Buy)
      (time 10:05:00.000000000) (liquidity Taker)))
    |}]
;;

let%expect_test "orders in the same bar share the participation budget" =
  (* First order takes 4,000 of the 4,200 budget (impact 10c *
     sqrt(4000/42000) = 3.09c -> 3c); the second gets only the 200 left
     (impact 10c * sqrt(200/42000) = 0.69c -> 1c). *)
  let t = fresh () in
  let t, first =
    Fill_model.on_child_order
      t
      (market_child ~id:1 ~side:Side.Buy ~quantity:4000)
  in
  let (_ : Fill_model.t), second =
    Fill_model.on_child_order
      t
      (market_child ~id:2 ~side:Side.Buy ~quantity:1000)
  in
  print_fills first;
  print_fills second;
  [%expect
    {|
    (((fill_id 1) (symbol NVDA) (price 15025) (size 4000) (order_id 1) (side Buy)
      (time 10:05:00.000000000) (liquidity Taker)))
    (((fill_id 2) (symbol NVDA) (price 15023) (size 200) (order_id 2) (side Buy)
      (time 10:05:00.000000000) (liquidity Taker)))
    |}]
;;

let%expect_test "a marketable limit fills like a taker but never beyond its \
                 limit"
  =
  (* Limit 150.30 crosses the 150.22 ask: fills at ask + 2c impact = 150.24,
     inside the limit. A tighter 150.23 limit caps the price. *)
  let (_ : Fill_model.t), fills =
    Fill_model.on_child_order
      (fresh ())
      (limit_child ~id:1 ~side:Side.Buy ~quantity:1000 ~limit:15030)
  in
  print_fills fills;
  [%expect
    {|
    (((fill_id 1) (symbol NVDA) (price 15024) (size 1000) (order_id 1) (side Buy)
      (time 10:05:00.000000000) (liquidity Taker)))
    |}];
  let (_ : Fill_model.t), fills =
    Fill_model.on_child_order
      (fresh ())
      (limit_child ~id:2 ~side:Side.Buy ~quantity:1000 ~limit:15023)
  in
  print_fills fills;
  [%expect
    {|
    (((fill_id 1) (symbol NVDA) (price 15023) (size 1000) (order_id 2) (side Buy)
      (time 10:05:00.000000000) (liquidity Taker)))
    |}]
;;

let%expect_test "a non-marketable limit rests without filling" =
  let (_ : Fill_model.t), fills =
    Fill_model.on_child_order
      (fresh ())
      (limit_child ~id:1 ~side:Side.Buy ~quantity:1000 ~limit:15015)
  in
  print_fills fills;
  [%expect {| () |}]
;;

let%expect_test "a resting limit fills at its own price when the bar trades \
                 strictly through"
  =
  (* Resting buy at 150.15; the bar's low is 150.10 < 150.15. Fills at the
     limit price as the Maker. *)
  let (_ : Fill_model.t), fills =
    Fill_model.on_bar_advance
      (Fill_model.create config)
      ~bar:(bar ())
      ~resting_orders:
        [ limit_child ~id:7 ~side:Side.Buy ~quantity:2000 ~limit:15015 ]
  in
  print_fills fills;
  [%expect
    {|
    (((fill_id 1) (symbol NVDA) (price 15015) (size 2000) (order_id 7) (side Buy)
      (time 10:05:00.000000000) (liquidity Maker)))
    |}]
;;

let%expect_test "a touch is not enough: the bar must trade through" =
  (* Resting buy at 150.10 and the bar's low is exactly 150.10. *)
  let (_ : Fill_model.t), fills =
    Fill_model.on_bar_advance
      (Fill_model.create config)
      ~bar:(bar ())
      ~resting_orders:
        [ limit_child ~id:7 ~side:Side.Buy ~quantity:2000 ~limit:15010 ]
  in
  print_fills fills;
  [%expect {| () |}]
;;

let%expect_test "sells mirror buys" =
  (* Market sell: bid = open - 2c = 150.18, impact 3c -> 150.15, Taker.
     Resting sell at 150.40: bar high 150.45 > 150.40 -> Maker fill. *)
  let (_ : Fill_model.t), fills =
    Fill_model.on_child_order
      (fresh ())
      (market_child ~id:1 ~side:Side.Sell ~quantity:5000)
  in
  print_fills fills;
  [%expect
    {|
    (((fill_id 1) (symbol NVDA) (price 15015) (size 4200) (order_id 1)
      (side Sell) (time 10:05:00.000000000) (liquidity Taker)))
    |}];
  let (_ : Fill_model.t), fills =
    Fill_model.on_bar_advance
      (Fill_model.create config)
      ~bar:(bar ())
      ~resting_orders:
        [ limit_child ~id:2 ~side:Side.Sell ~quantity:1000 ~limit:15040 ]
  in
  print_fills fills;
  [%expect
    {|
    (((fill_id 1) (symbol NVDA) (price 15040) (size 1000) (order_id 2)
      (side Sell) (time 10:05:00.000000000) (liquidity Maker)))
    |}]
;;

let%expect_test "zero-volume bars fill nothing" =
  let (_ : Fill_model.t), fills =
    Fill_model.on_child_order
      (fresh ~volume:0 ())
      (market_child ~id:1 ~side:Side.Buy ~quantity:100)
  in
  print_fills fills;
  [%expect {| () |}]
;;

let%expect_test "the synthetic bbo brackets the open with the remaining \
                 budget as depth"
  =
  print_endline
    (Bbo.to_string (Option.value_exn (Fill_model.synthetic_bbo (fresh ()))));
  [%expect {| $150.18 x4200 / $150.22 x4200 |}]
;;

let%expect_test "submitting before any bar raises" =
  Expect_test_helpers_core.require_does_raise (fun () ->
    Fill_model.on_child_order
      (Fill_model.create config)
      (market_child ~id:1 ~side:Side.Buy ~quantity:100));
  [%expect
    {| ("Fill_model: no bar has been seen yet" (here on_child_order)) |}]
;;

let%expect_test "a market order in the resting list is a driver bug" =
  Expect_test_helpers_core.require_does_raise (fun () ->
    Fill_model.on_bar_advance
      (Fill_model.create config)
      ~bar:(bar ())
      ~resting_orders:[ market_child ~id:1 ~side:Side.Buy ~quantity:100 ]);
  [%expect {| ("Fill_model: a market order cannot rest" (order_id 1)) |}]
;;
