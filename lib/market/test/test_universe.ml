open! Core
open! Execlab_types
open Execlab_market

let day ~symbol ~date ~cents =
  let price = Price.of_int_cents cents in
  Or_error.ok_exn
    (Trading_day.create
       ~symbol:(Symbol.of_string symbol)
       ~date:(Date.of_string date)
       ~bars:
         (List.init 390 ~f:(fun minute ->
            Or_error.ok_exn
              (Market_bar.create
                 ~time:
                   (Option.value_exn
                      (Time_ns.Ofday.add
                         (Time_ns.Ofday.of_string "09:30:00")
                         (Time_ns.Span.of_int_min minute)))
                 ~open_:price
                 ~high:price
                 ~low:price
                 ~close:price
                 ~volume:(Size.of_int 1000)))))
;;

let%expect_test "a universe of two symbols steps them on one clock" =
  let universe =
    Or_error.ok_exn
      (Universe.of_days
         [ day ~symbol:"TSLA" ~date:"2026-07-09" ~cents:39400
         ; day ~symbol:"AAPL" ~date:"2026-07-09" ~cents:22500
         ])
  in
  print_s [%sexp (Universe.symbols universe : Symbol.t list)];
  printf "minutes: %d\n" (Universe.minutes universe);
  printf
    "minute 30 is %s for both\n"
    (Time_ns.Ofday.to_string (Universe.time_at universe ~minute:30));
  List.iter (Universe.symbols universe) ~f:(fun symbol ->
    printf
      "  %s opens minute 30 at %s\n"
      (Symbol.to_string symbol)
      (Price.to_string_dollar
         (Universe.bar_exn universe ~symbol ~minute:30).open_));
  [%expect
    {|
    (AAPL TSLA)
    minutes: 390
    minute 30 is 10:00:00.000000000 for both
      AAPL opens minute 30 at $225.00
      TSLA opens minute 30 at $394.00
    |}]
;;

let%expect_test "sessions must agree on the date, and a symbol may not \
                 appear twice"
  =
  let show result =
    print_s [%sexp (Or_error.ignore_m result : unit Or_error.t)]
  in
  show
    (Universe.of_days
       [ day ~symbol:"TSLA" ~date:"2026-07-09" ~cents:39400
       ; day ~symbol:"AAPL" ~date:"2026-07-10" ~cents:22500
       ]);
  show
    (Universe.of_days
       [ day ~symbol:"TSLA" ~date:"2026-07-09" ~cents:39400
       ; day ~symbol:"TSLA" ~date:"2026-07-09" ~cents:39500
       ]);
  show (Universe.of_days []);
  [%expect
    {|
    (Error
     ("Universe.of_days: sessions are not the same date" (first 2026-07-09)
      (also 2026-07-10)))
    (Error ("Universe.of_days: two sessions for one symbol" (symbol TSLA)))
    (Error "Universe.of_days: no sessions")
    |}]
;;

let%expect_test "reaching for a symbol the run never loaded raises" =
  let universe =
    Universe.of_day (day ~symbol:"TSLA" ~date:"2026-07-09" ~cents:39400)
  in
  printf "mem TSLA: %b\n" (Universe.mem universe (Symbol.of_string "TSLA"));
  printf "mem AAPL: %b\n" (Universe.mem universe (Symbol.of_string "AAPL"));
  Expect_test_helpers_core.show_raise (fun () ->
    Universe.bar_exn universe ~symbol:(Symbol.of_string "AAPL") ~minute:0);
  [%expect
    {|
    mem TSLA: true
    mem AAPL: false
    (raised (
      "Universe: symbol is not in this run" (symbol AAPL) (universe (TSLA))))
    |}]
;;
