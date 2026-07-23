open! Core
open Execlab_types

let%expect_test "arithmetic" =
  let a = Size.of_int 300 in
  let b = Size.of_int 200 in
  print_s [%sexp (Size.( + ) a b : Size.t)];
  print_s [%sexp (Size.( - ) a b : Size.t)];
  print_s [%sexp (Size.( * ) a 3 : Size.t)];
  [%expect {|
    500
    100
    900
    |}]
;;
