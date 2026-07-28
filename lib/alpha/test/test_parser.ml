open! Core
open! Execlab_types
open Execlab_alpha

let parse_and_print csv =
  print_s [%sexp (Parser.parse csv : Parser.t Or_error.t)]
;;

let%expect_test "parse valid csv" =
  parse_and_print
    "09:30:00,AAPL,BUY,100,10:15:00\n09:30:01,GOOG,SELL,200,10:15:01";
  [%expect
    {|
    (Ok
     ((instructions
       (((arrival_time 09:30:00.000000000) (symbol AAPL) (side Buy)
         (quantity 100) (deadline 10:15:00.000000000))
        ((arrival_time 09:30:01.000000000) (symbol GOOG) (side Sell)
         (quantity 200) (deadline 10:15:01.000000000))))))
    |}]
;;

let%expect_test "side is case-insensitive" =
  parse_and_print "09:30:00,AAPL,buy,100,10:15:00";
  [%expect
    {|
    (Ok
     ((instructions
       (((arrival_time 09:30:00.000000000) (symbol AAPL) (side Buy)
         (quantity 100) (deadline 10:15:00.000000000))))))
    |}]
;;

let%expect_test "trailing newline is fine" =
  parse_and_print "09:30:00,AAPL,BUY,100,10:15:00\n";
  [%expect
    {|
    (Ok
     ((instructions
       (((arrival_time 09:30:00.000000000) (symbol AAPL) (side Buy)
         (quantity 100) (deadline 10:15:00.000000000))))))
    |}]
;;

let%expect_test "empty input gives no instructions" =
  parse_and_print "";
  [%expect {| (Ok ((instructions ()))) |}]
;;

let%expect_test "bad side is an error" =
  parse_and_print "09:30:00,AAPL,HOLD,100,10:15:00";
  [%expect {| (Error (Failure "Line 1: invalid side 'HOLD'")) |}]
;;

let%expect_test "wrong number of fields is an error" =
  parse_and_print "09:30:00,AAPL,BUY,100";
  [%expect {| (Error (Failure "Line 1: expected 5 fields")) |}]
;;

let%expect_test "bad quantity is an error" =
  parse_and_print "09:30:00,AAPL,BUY,lots,10:15:00";
  [%expect {| (Error (Failure "Int.of_string: \"lots\"")) |}]
;;

let%expect_test "bad time is an error" =
  parse_and_print "not-a-time,AAPL,BUY,100,10:15:00";
  [%expect {| (Error (Failure "Char.get_digit_exn 'o': not a digit")) |}]
;;

let%expect_test "deadline before arrival is an error" =
  parse_and_print "10:15:00,AAPL,BUY,100,09:30:00";
  [%expect
    {|
    (Error
     ("Arrival time must be before or equal to deadline"
      (arrival_time 10:15:00.000000000) (deadline 09:30:00.000000000)))
    |}]
;;

let%expect_test "zero quantity is an error" =
  parse_and_print "09:30:00,AAPL,BUY,0,10:15:00";
  [%expect {| (Error ("Quantity must be positive" (quantity 0))) |}]
;;

let%expect_test "outside market hours is an error" =
  parse_and_print "03:00:00,AAPL,BUY,100,10:15:00";
  [%expect
    {|
    (Error
     ("Arrival time must be after or equal to 09:30:00"
      (arrival_time 03:00:00.000000000)))
    |}]
;;

let%expect_test "bad line in a multi-line csv" =
  parse_and_print
    "09:30:00,AAPL,BUY,100,10:15:00\n09:30:01,GOOG,SELL,-5,10:15:01";
  [%expect {| (Error ("Quantity must be positive" (quantity -5))) |}]
;;
