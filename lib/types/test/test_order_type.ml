open! Core
open Execlab_types

let%expect_test "to_string" =
  print_endline (Order_type.to_string Market);
  print_endline (Order_type.to_string (Limit (Price.of_int_cents 15025)));
  [%expect {|
    MKT
    LMT $150.25
    |}]
;;
