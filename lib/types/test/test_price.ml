open! Core
open Sandbox_types

let%expect_test "to_string_dollar" =
  print_endline (Price.to_string_dollar (Price.of_int_cents 15025));
  [%expect {| $150.25 |}]
;;
