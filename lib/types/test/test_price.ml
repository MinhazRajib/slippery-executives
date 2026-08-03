open! Core
open Execlab_types

let%expect_test "to_string_dollar" =
  print_endline (Price.to_string_dollar (Price.of_int_cents 15025));
  print_endline (Price.to_string_dollar (Price.of_int_cents (-5)));
  print_endline (Price.to_string_dollar Price.zero);
  [%expect {|
    $150.25
    -$0.05
    $0.00
    |}]
;;

let%expect_test "of_string accepts an optional dollar sign" =
  print_s [%sexp (Price.of_string "$150.25" : Price.t)];
  print_s [%sexp (Price.of_string "150.25" : Price.t)];
  [%expect {|
    15025
    15025
    |}]
;;

let%expect_test "arithmetic" =
  let p = Price.of_int_cents 15025 in
  let q = Price.of_int_cents 50 in
  print_s [%sexp (Price.( + ) p q : Price.t)];
  print_s [%sexp (Price.( - ) p q : Price.t)];
  print_s [%sexp (Price.( * ) q 3 : Price.t)];
  [%expect {|
    15075
    14975
    150
    |}]
;;

let%expect_test "aggressiveness and marketability are side-relative" =
  let high = Price.of_int_cents 15030 in
  let low = Price.of_int_cents 15020 in
  List.iter Side.all ~f:(fun side ->
    printf
      "%s: higher more_aggressive=%b, marketable at resting=%b\n"
      (Side.to_string side)
      (Price.is_more_aggressive side ~price:high ~than:low)
      (Price.is_marketable side ~price:high ~resting_price:low));
  (* Equal prices: not more aggressive, but marketable for both sides. *)
  printf
    "equal: more_aggressive=%b, marketable=%b\n"
    (Price.is_more_aggressive Side.Buy ~price:low ~than:low)
    (Price.is_marketable Side.Buy ~price:low ~resting_price:low);
  [%expect
    {|
    BUY: higher more_aggressive=true, marketable at resting=true
    SELL: higher more_aggressive=false, marketable at resting=false
    equal: more_aggressive=false, marketable=true
    |}]
;;

let%expect_test "of_float_exn accepts exact cent values" =
  (* 1.15 and 0.07 are not exactly representable as floats; scaling by 100
     gives e.g. 114.99999999999999. They must still be accepted. *)
  List.iter [ 150.25; 1.15; 0.07; -0.05; 0. ] ~f:(fun f ->
    printf "%.2f -> %d cents\n" f (Price.to_int_cents (Price.of_float_exn f)));
  [%expect
    {|
    150.25 -> 15025 cents
    1.15 -> 115 cents
    0.07 -> 7 cents
    -0.05 -> -5 cents
    0.00 -> 0 cents
    |}]
;;

let%expect_test "of_float_round_nearest rounds to the nearest cent" =
  (* Exact half-cents round toward positive infinity ([Float.round_nearest]
     is [floor (x +. 0.5)]), hence the +1/0 asymmetry below. *)
  List.iter [ 395.538; 394.9927; 150.25; 0.005; -0.005 ] ~f:(fun f ->
    printf
      "%.4f -> %d cents\n"
      f
      (Price.to_int_cents (Price.of_float_round_nearest f)));
  [%expect
    {|
    395.5380 -> 39554 cents
    394.9927 -> 39499 cents
    150.2500 -> 15025 cents
    0.0050 -> 1 cents
    -0.0050 -> 0 cents
    |}];
  Expect_test_helpers_core.show_raise (fun () ->
    Price.of_float_round_nearest Float.nan);
  [%expect
    {|
    (raised (
      Invalid_argument "Int.of_float: argument (nan) is out of range or NaN"))
    |}]
;;

let%expect_test "of_float_exn rejects sub-cent values" =
  List.iter [ 150.001; 150.009; 99.999 ] ~f:(fun f ->
    Expect_test_helpers_core.show_raise (fun () -> Price.of_float_exn f));
  [%expect
    {|
    (raised ("Price.of_float_exn: not representable as exact cents" (f 150.001)))
    (raised ("Price.of_float_exn: not representable as exact cents" (f 150.009)))
    (raised ("Price.of_float_exn: not representable as exact cents" (f 99.999)))
    |}]
;;
