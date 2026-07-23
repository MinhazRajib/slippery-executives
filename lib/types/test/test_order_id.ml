open! Core
open Execlab_types

let%expect_test "generator assigns sequential ids starting at 1" =
  let generator = Order_id.Generator.create () in
  List.iter [ 1; 2; 3 ] ~f:(fun (_ : int) ->
    print_s [%sexp (Order_id.Generator.next generator : Order_id.t)]);
  [%expect {|
    1
    2
    3
    |}]
;;
