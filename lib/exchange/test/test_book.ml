open! Core
open! Execlab_types
open Execlab_exchange

let cents = Price.of_int_cents
let show book = print_s [%sexp (book : Book.t)]

let ladder =
  Book.set_side
    (Book.set_side
       Book.empty
       ~side:Sell
       [ cents 10003, 80; cents 10002, 50; cents 10004, 70 ])
    ~side:Buy
    [ cents 9998, 60; cents 9997, 90 ]
;;

let%expect_test "sides sort best-first and aggregate duplicates" =
  show ladder;
  [%expect
    {| ((bids ((9998 60) (9997 90))) (asks ((10002 50) (10003 80) (10004 70)))) |}];
  show (Book.set_side Book.empty ~side:Sell [ cents 100, 10; cents 100, 15 ]);
  [%expect {| ((bids ()) (asks ((100 25)))) |}]
;;

let%expect_test "a market buy walks the asks best-first: 60 = 50@100.02 + \
                 10@100.03"
  =
  let book, fills = Book.take ladder ~taker_side:Buy ~size:60 () in
  print_s [%sexp (fills : (Price.t * int) list)];
  show book;
  [%expect
    {|
    ((10002 50) (10003 10))
    ((bids ((9998 60) (9997 90))) (asks ((10003 70) (10004 70))))
    |}]
;;

let%expect_test "a limit buy stops at its price: 100.02 only" =
  let (_ : Book.t), fills =
    Book.take ladder ~taker_side:Buy ~limit:(cents 10002) ~size:60 ()
  in
  print_s [%sexp (fills : (Price.t * int) list)];
  [%expect {| ((10002 50)) |}]
;;

let%expect_test "depth exhaustion fills what exists: 200 wanted, 200 shown" =
  let book, fills = Book.take ladder ~taker_side:Buy ~size:500 () in
  print_s [%sexp (fills : (Price.t * int) list)];
  printf "asks left: %b\n" (Option.is_some (Book.best book ~side:Sell));
  [%expect
    {|
    ((10002 50) (10003 80) (10004 70))
    asks left: false
    |}]
;;

let%expect_test "a market sell hits the bids downward" =
  let (_ : Book.t), fills = Book.take ladder ~taker_side:Sell ~size:100 () in
  print_s [%sexp (fills : (Price.t * int) list)];
  [%expect {| ((9998 60) (9997 40)) |}]
;;

let%expect_test "size_at reports what a client order would queue behind" =
  (* Only the exact price matters for queue position: better prices decide
     whether the market reaches you at all, not who is ahead of you when it
     does. *)
  let book =
    Book.set_side
      Book.empty
      ~side:Side.Buy
      [ Price.of_int_cents 9_999, 500; Price.of_int_cents 9_998, 1_200 ]
  in
  let at side cents =
    Book.size_at book ~side ~price:(Price.of_int_cents cents)
  in
  printf "bid 99.99: %d\n" (at Side.Buy 9_999);
  printf "bid 99.98: %d\n" (at Side.Buy 9_998);
  printf "bid 99.97 (nobody there): %d\n" (at Side.Buy 9_997);
  printf "ask 99.99 (wrong side): %d\n" (at Side.Sell 9_999);
  [%expect
    {|
    bid 99.99: 500
    bid 99.98: 1200
    bid 99.97 (nobody there): 0
    ask 99.99 (wrong side): 0
    |}]
;;
