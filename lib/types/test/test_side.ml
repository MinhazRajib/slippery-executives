open! Core
open Execlab_types

let%expect_test "to_string and case-insensitive of_string" =
  List.iter Side.all ~f:(fun side -> print_endline (Side.to_string side));
  [%expect {|
    BUY
    SELL
    |}];
  print_s [%sexp (Side.of_string "buy" : Side.t)];
  print_s [%sexp (Side.of_string "Sell" : Side.t)];
  [%expect {|
    Buy
    Sell
    |}]
;;

let%expect_test "flip and sign" =
  List.iter Side.all ~f:(fun side ->
    print_s
      [%message
        (side : Side.t) (Side.flip side : Side.t) (Side.sign side : int)]);
  [%expect
    {|
    ((side Buy) ("Side.flip side" Sell) ("Side.sign side" 1))
    ((side Sell) ("Side.flip side" Buy) ("Side.sign side" -1))
    |}]
;;
