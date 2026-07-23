open! Core
open Execlab_types

let create ~arrival_time ~deadline ~quantity =
  Alpha_instruction.create
    ~arrival_time:(Time_ns.Ofday.of_string arrival_time)
    ~symbol:(Symbol.of_string "NVDA")
    ~side:Side.Buy
    ~quantity:(Size.of_int quantity)
    ~deadline:(Time_ns.Ofday.of_string deadline)
;;

let print result = print_s [%sexp (result : Alpha_instruction.t Or_error.t)]

let%expect_test "create accepts a valid instruction" =
  print
    (create ~arrival_time:"10:05:00" ~deadline:"11:00:00" ~quantity:50000);
  [%expect
    {|
    (Ok
     ((arrival_time 10:05:00.000000000) (symbol NVDA) (side Buy) (quantity 50000)
      (deadline 11:00:00.000000000)))
    |}]
;;

let%expect_test "deadline equal to arrival time is allowed" =
  print (create ~arrival_time:"10:05:00" ~deadline:"10:05:00" ~quantity:100);
  [%expect
    {|
    (Ok
     ((arrival_time 10:05:00.000000000) (symbol NVDA) (side Buy) (quantity 100)
      (deadline 10:05:00.000000000)))
    |}]
;;

let%expect_test "quantity must be positive" =
  print (create ~arrival_time:"10:05:00" ~deadline:"11:00:00" ~quantity:0);
  [%expect {| (Error ("Quantity must be positive" (quantity 0))) |}]
;;

let%expect_test "deadline must not precede arrival time" =
  print (create ~arrival_time:"11:00:00" ~deadline:"10:05:00" ~quantity:100);
  [%expect
    {|
    (Error
     ("Arrival time must be before or equal to deadline"
      (arrival_time 11:00:00.000000000) (deadline 10:05:00.000000000)))
    |}]
;;

let%expect_test "times must be within market hours" =
  print (create ~arrival_time:"09:00:00" ~deadline:"11:00:00" ~quantity:100);
  print (create ~arrival_time:"10:05:00" ~deadline:"16:30:00" ~quantity:100);
  [%expect
    {|
    (Error
     ("Arrival time must be after or equal to 09:30:00"
      (arrival_time 09:00:00.000000000)))
    (Error
     ("Deadline must be before or equal to 16:00:00"
      (deadline 16:30:00.000000000)))
    |}]
;;
