open! Core
open Execlab_types

let bid = { Level.price = Price.of_int_cents 15020; size = Size.of_int 300 }
let ask = { Level.price = Price.of_int_cents 15022; size = Size.of_int 500 }
let bbo = { Bbo.bid = Some bid; ask = Some ask }

let%expect_test "to_string" =
  print_endline (Bbo.to_string bbo);
  print_endline (Bbo.to_string Bbo.empty);
  [%expect {|
    $150.20 x300 / $150.22 x500
    - / -
    |}]
;;

let%expect_test "spread" =
  print_s [%sexp (Bbo.spread bbo : Price.t option)];
  print_s [%sexp (Bbo.spread Bbo.empty : Price.t option)];
  print_s
    [%sexp (Bbo.spread { Bbo.bid = Some bid; ask = None } : Price.t option)];
  [%expect {|
    (2)
    ()
    ()
    |}]
;;

let%expect_test "price and size are book-side accessors" =
  (* [Buy] means the bid side of the book, not "the price a buyer pays". *)
  List.iter Side.all ~f:(fun side ->
    print_s
      [%message
        (side : Side.t)
          (Bbo.price bbo side : Price.t option)
          (Bbo.size bbo side : Size.t option)]);
  [%expect
    {|
    ((side Buy) ("Bbo.price bbo side" (15020)) ("Bbo.size bbo side" (300)))
    ((side Sell) ("Bbo.price bbo side" (15022)) ("Bbo.size bbo side" (500)))
    |}]
;;
