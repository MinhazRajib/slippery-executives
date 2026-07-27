open! Core
open! Execlab_types
open Execlab_market

let time_at_minute i =
  Option.value_exn
    (Time_ns.Ofday.add
       (Time_ns.Ofday.of_string "09:30:00")
       (Time_ns.Span.of_int_min i))
;;

let bar_at_minute i =
  Or_error.ok_exn
    (Market_bar.create
       ~time:(time_at_minute i)
       ~open_:(Price.of_int_cents 39400)
       ~high:(Price.of_int_cents 39500)
       ~low:(Price.of_int_cents 39300)
       ~close:(Price.of_int_cents 39450)
       ~volume:(Size.of_int 1000))
;;

let full_day = List.init 390 ~f:bar_at_minute

let create bars =
  Trading_day.create
    ~symbol:(Symbol.of_string "TSLA")
    ~date:(Date.of_string "2026-07-09")
    ~bars
;;

let print_error result = print_s [%sexp (result : Trading_day.t Or_error.t)]

let%expect_test "create accepts a full session" =
  let day = Or_error.ok_exn (create full_day) in
  printf
    "%s %s with %d bars\n"
    (Symbol.to_string day.Trading_day.symbol)
    (Date.to_string day.Trading_day.date)
    (List.length day.Trading_day.bars);
  [%expect {| TSLA 2026-07-09 with 390 bars |}]
;;

let%expect_test "bar count must cover the whole session" =
  print_error (create (List.take full_day 10));
  [%expect
    {|
    (Error
     ("Bars list must contain one bar per minute of the session" (bar_count 10)
      (bars_per_session 390)))
    |}]
;;

let%expect_test "bars must be sorted by time" =
  print_error (create (List.rev full_day));
  [%expect
    {| (Error "Bars list must be sorted by strictly increasing time") |}]
;;

let%expect_test "duplicate times are rejected" =
  print_error
    (create
       (List.mapi full_day ~f:(fun i b ->
          if i = 1 then bar_at_minute 0 else b)));
  [%expect
    {| (Error "Bars list must be sorted by strictly increasing time") |}]
;;

let%expect_test "first bar must be at the open" =
  print_error (create (List.init 390 ~f:(fun i -> bar_at_minute (i + 1))));
  [%expect
    {|
    (Error
     ("First bar must be at the session open" (first_bar_time 09:31:00.000000000)
      (session_first_bar 09:30:00.000000000)))
    |}]
;;

let%expect_test "last bar must be at the final session minute" =
  print_error (create (List.take full_day 389 @ [ bar_at_minute 420 ]));
  [%expect
    {|
    (Error
     ("Last bar must be at the final session minute"
      (last_bar_time 16:30:00.000000000) (session_last_bar 15:59:00.000000000)))
    |}]
;;
