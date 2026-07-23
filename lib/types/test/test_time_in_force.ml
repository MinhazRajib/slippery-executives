open! Core
open Execlab_types

let%expect_test "rests_on_book" =
  List.iter Time_in_force.all ~f:(fun tif ->
    printf
      "%s rests=%b\n"
      (Time_in_force.to_string tif)
      (Time_in_force.rests_on_book tif));
  [%expect {|
    DAY rests=true
    IOC rests=false
    |}]
;;

let%expect_test "of_string is case-insensitive" =
  print_s [%sexp (Time_in_force.of_string "day" : Time_in_force.t)];
  print_s [%sexp (Time_in_force.of_string "ioc" : Time_in_force.t)];
  [%expect {|
    Day
    IOC
    |}]
;;

let%expect_test "all_str lists every value for error messages" =
  print_endline Time_in_force.all_str;
  [%expect {| DAY, IOC |}]
;;
