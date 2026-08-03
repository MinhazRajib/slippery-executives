open! Core
open! Execlab_types
open Execlab_market
open Execlab_analytics

(* A flat $10.00 session: every bar opens at the arrival price, so the
   report's timing cost line is exactly $0.00 here. *)
let day =
  let bar_at_minute i =
    let price = Price.of_int_cents 1000 in
    Or_error.ok_exn
      (Market_bar.create
         ~time:
           (Option.value_exn
              (Time_ns.Ofday.add
                 (Time_ns.Ofday.of_string "09:30:00")
                 (Time_ns.Span.of_int_min i)))
         ~open_:price
         ~high:price
         ~low:price
         ~close:price
         ~volume:(Size.of_int 1000))
  in
  Or_error.ok_exn
    (Trading_day.create
       ~symbol:(Symbol.of_string "NVDA")
       ~date:(Date.of_string "2026-07-09")
       ~bars:(List.init 390 ~f:bar_at_minute))
;;

let instruction =
  Or_error.ok_exn
    (Alpha_instruction.create
       ~arrival_time:(Time_ns.Ofday.of_string "10:00:00")
       ~symbol:(Symbol.of_string "NVDA")
       ~side:Buy
       ~quantity:(Size.of_int 100)
       ~deadline:(Time_ns.Ofday.of_string "11:00:00"))
;;

let fill size price_cents =
  { Fill.fill_id = 0
  ; symbol = Symbol.of_string "NVDA"
  ; price = Price.of_int_cents price_cents
  ; size = Size.of_int size
  ; order_id = Order_id.For_testing.of_int 1
  ; side = Buy
  ; time = Time_ns.Ofday.of_string "10:15:00"
  ; liquidity = Liquidity.Taker
  }
;;

let grade fills =
  Or_error.ok_exn
    (Transaction_cost.create
       ~instruction
       ~fills
       ~day
       ~arrival_price:(Price.of_int_cents 1000)
       ~terminal_price:(Price.of_int_cents 1100)
       ~day_vwap:10.15
       ~half_spread:(Price.of_int_cents 1))
;;

let%expect_test "the full block, matching the mli example" =
  print_endline (Report.to_string_hum (grade [ fill 50 1010; fill 50 1030 ]));
  [%expect
    {|
    NVDA BUY 100 shares
      filled          100 of 100 (100.0%)
      arrival         $10.00
      terminal        $11.00
      day vwap        $10.1500
      avg fill        $10.2000
      shortfall       +200.0 bps ($20.00)
      vs day vwap     +49.3 bps
      timing cost     $0.00
      spread cost     $1.00
      impact cost     $19.00
      gross alpha     $100.00
      net P&L         $80.00
      opportunity     $0.00
      alpha captured  80.0%
    |}]
;;

let%expect_test "an unfilled instruction still reports" =
  print_endline (Report.to_string_hum (grade []));
  [%expect
    {|
    NVDA BUY 100 shares
      filled          0 of 100 (0.0%)
      arrival         $10.00
      terminal        $11.00
      day vwap        $10.1500
      no fills
      gross alpha     $100.00
      net P&L         $0.00
      opportunity     $100.00
      alpha captured  0.0%
    |}]
;;

let%expect_test "comparison: two gradings and the value-add line" =
  print_endline
    (Or_error.ok_exn
       (Report.comparison
          ~algo:(grade [ fill 100 1010 ])
          ~algo_name:"TWAP"
          ~baseline:(grade [ fill 100 1030 ])
          ~baseline_name:"immediate"));
  [%expect
    {|
    === TWAP ===
    NVDA BUY 100 shares
      filled          100 of 100 (100.0%)
      arrival         $10.00
      terminal        $11.00
      day vwap        $10.1500
      avg fill        $10.1000
      shortfall       +100.0 bps ($10.00)
      vs day vwap     -49.3 bps
      timing cost     $0.00
      spread cost     $1.00
      impact cost     $9.00
      gross alpha     $100.00
      net P&L         $90.00
      opportunity     $0.00
      alpha captured  90.0%
    === immediate ===
    NVDA BUY 100 shares
      filled          100 of 100 (100.0%)
      arrival         $10.00
      terminal        $11.00
      day vwap        $10.1500
      avg fill        $10.3000
      shortfall       +300.0 bps ($30.00)
      vs day vwap     +147.8 bps
      timing cost     $0.00
      spread cost     $1.00
      impact cost     $29.00
      gross alpha     $100.00
      net P&L         $70.00
      opportunity     $0.00
      alpha captured  70.0%
    value added (TWAP - immediate): $20.00
    |}]
;;
