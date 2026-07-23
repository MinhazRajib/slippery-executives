open! Core
open Execlab_types

let fill =
  { Fill.fill_id = 3
  ; symbol = Symbol.of_string "NVDA"
  ; price = Price.of_int_cents 15026
  ; size = Size.of_int 700
  ; order_id = Order_id.For_testing.of_int 42
  ; side = Side.Buy
  ; time = Time_ns.Ofday.of_string "10:15:08"
  ; liquidity = Liquidity.Taker
  }
;;

let%expect_test "to_string reads like an event-log line" =
  print_endline (Fill.to_string fill);
  [%expect
    {| 10:15:08.000000000 BUY 700 NVDA @ $150.26 (Taker, fill 3, order 42) |}]
;;

let%expect_test "sexp and notional" =
  print_s [%sexp (fill : Fill.t)];
  print_s [%sexp (Fill.notional_cents fill : int)];
  [%expect
    {|
    ((fill_id 3) (symbol NVDA) (price 15026) (size 700) (order_id 42) (side Buy)
     (time 10:15:08.000000000) (liquidity Taker))
    10518200
    |}]
;;
