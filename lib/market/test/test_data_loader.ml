open! Core
open! Execlab_types
open Execlab_market

let header = "time,open,high,low,close,volume"

let time_at_minute i =
  Option.value_exn
    (Time_ns.Ofday.add
       (Time_ns.Ofday.of_string "09:30:00")
       (Time_ns.Span.of_int_min i))
;;

let row_at_minute i =
  [%string
    "%{time_at_minute i#Time_ns.Ofday},394.00,395.538,393.10,394.9927,1000"]
;;

let full_csv =
  String.concat ~sep:"\n" (header :: List.init 390 ~f:row_at_minute)
;;

let parse contents =
  Data_loader.parse
    ~symbol:(Symbol.of_string "TSLA")
    ~date:(Date.of_string "2026-07-09")
    contents
;;

let print_error result = print_s [%sexp (result : Trading_day.t Or_error.t)]

let%expect_test "parse accepts a full session and rounds sub-cent prices" =
  let day = Or_error.ok_exn (parse full_csv) in
  printf
    "%s %s with %d bars\n"
    (Symbol.to_string day.Trading_day.symbol)
    (Date.to_string day.Trading_day.date)
    (List.length day.Trading_day.bars);
  print_s [%sexp (List.hd_exn day.Trading_day.bars : Market_bar.t)];
  [%expect
    {|
    TSLA 2026-07-09 with 390 bars
    ((time 09:30:00.000000000) (open_ 39400) (high 39554) (low 39310)
     (close 39499) (volume 1000))
    |}]
;;

let%expect_test "header must match exactly" =
  print_error (parse "timestamp,open,high,low,close,volume\nwhatever");
  [%expect
    {|
    (Error
     ("Unexpected header" (header timestamp,open,high,low,close,volume)
      (expected_header time,open,high,low,close,volume)))
    |}]
;;

let%expect_test "empty file" =
  print_error (parse "");
  [%expect {| (Error "Market data file is empty") |}]
;;

let%expect_test "bad rows are reported with line numbers, all at once" =
  print_error
    (parse
       (String.concat
          ~sep:"\n"
          [ header
          ; "09:30:00,394.00,395.00"
          ; "09:31:00,notaprice,395.00,393.10,394.00,1000"
          ; "09:32:00,394.00,395.00,393.10,394.00,1000"
          ]));
  [%expect
    {|
    (Error
     ((("Invalid bar row" (line_number 2) (line 09:30:00,394.00,395.00))
       ("expected 6 comma-separated fields" ("List.length fields" 3)))
      (("Invalid bar row" (line_number 3)
        (line 09:31:00,notaprice,395.00,393.10,394.00,1000))
       (Invalid_argument "Float.of_string notaprice"))))
    |}]
;;

let%expect_test "an invalid bar is caught per row" =
  print_error
    (parse
       (String.concat
          ~sep:"\n"
          [ header; "09:30:00,394.00,393.00,393.10,394.00,1000" ]));
  [%expect
    {|
    (Error
     (("Invalid bar row" (line_number 2)
       (line 09:30:00,394.00,393.00,393.10,394.00,1000))
      ("High price must be greater than or equal to low price" (high 39300)
       (low 39310))))
    |}]
;;

let%expect_test "sequence validation is delegated to Trading_day" =
  print_error
    (parse
       (String.concat
          ~sep:"\n"
          [ header
          ; "09:30:00,394.00,395.00,393.10,394.00,1000"
          ; "09:31:00,394.00,395.00,393.10,394.00,1000"
          ]));
  [%expect
    {|
    (Error
     ("Bars list must contain one bar per minute of the session" (bar_count 2)
      (bars_per_session 390)))
    |}]
;;

let%expect_test "load reports a missing file as an error" =
  print_error
    (Data_loader.load
       ~data_dir:"no-such-dir"
       ~symbol:(Symbol.of_string "TSLA")
       ~date:(Date.of_string "2026-07-09")
       ());
  [%expect
    {|
    (Error
     ("Could not read market data file"
      (filename no-such-dir/TSLA/2026-07-09.csv)
      (message "no-such-dir/TSLA/2026-07-09.csv: No such file or directory")))
    |}]
;;
