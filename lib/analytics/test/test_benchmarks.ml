open! Core
open! Execlab_types
open Execlab_market
open Execlab_analytics

let time_at_minute i =
  Option.value_exn
    (Time_ns.Ofday.add
       (Time_ns.Ofday.of_string "09:30:00")
       (Time_ns.Span.of_int_min i))
;;

(* Minute [i] opens at 390.00 + i cents and closes one cent higher, so every
   minute's open and close are distinct and predictable. *)
let bar_at_minute i =
  let open_ = Price.of_int_cents (39000 + i) in
  let close = Price.of_int_cents (39001 + i) in
  Or_error.ok_exn
    (Market_bar.create
       ~time:(time_at_minute i)
       ~open_
       ~high:close
       ~low:open_
       ~close
       ~volume:(Size.of_int 1000))
;;

let day =
  Or_error.ok_exn
    (Trading_day.create
       ~symbol:(Symbol.of_string "TSLA")
       ~date:(Date.of_string "2026-07-09")
       ~bars:(List.init 390 ~f:bar_at_minute))
;;

let print_arrival arrival_time =
  match
    Benchmarks.arrival_price
      day
      ~arrival_time:(Time_ns.Ofday.of_string arrival_time)
  with
  | Ok price -> print_endline (Price.to_string_dollar price)
  | Error error -> print_s [%sexp (error : Error.t)]
;;

let%expect_test "arrival on a minute boundary: that minute's open" =
  print_arrival "10:00:00";
  [%expect {| $390.30 |}]
;;

let%expect_test "arrival between minutes: the next minute's open (the first \
                 price the market shows after the decision)"
  =
  print_arrival "10:00:30";
  [%expect {| $390.31 |}]
;;

let%expect_test "arrival after the final minute is an error" =
  print_arrival "15:59:30";
  [%expect
    {|
    ("Benchmarks.arrival_price: no session minute at or after arrival"
     (symbol TSLA) (arrival_time 15:59:30.000000000))
    |}]
;;

let%expect_test "terminal price is the final minute's close" =
  print_endline (Price.to_string_dollar (Benchmarks.terminal_price day));
  [%expect {| $393.90 |}]
;;
