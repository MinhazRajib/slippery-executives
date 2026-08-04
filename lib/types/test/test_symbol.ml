open! Core
open Execlab_types

let%expect_test "of_string accepts uppercase symbols" =
  print_s [%sexp (Symbol.of_string "NVDA" : Symbol.t)];
  [%expect {| NVDA |}]
;;

let%expect_test "of_string rejects empty and non-uppercase" =
  Expect_test_helpers_core.show_raise (fun () -> Symbol.of_string "");
  [%expect {| (raised "Symbol.of_string: symbol must be non-empty") |}];
  Expect_test_helpers_core.show_raise (fun () -> Symbol.of_string "nvda");
  [%expect {| (raised "Symbol.of_string: symbol must be uppercase") |}]
;;

let%expect_test "the sexp reader validates like of_string: wire input is \
                 untrusted"
  =
  print_s [%sexp (Symbol.t_of_sexp (Sexp.of_string "NVDA") : Symbol.t)];
  [%expect {| NVDA |}];
  Expect_test_helpers_core.show_raise (fun () ->
    Symbol.t_of_sexp (Sexp.of_string "\"../TSLA\""));
  [%expect {| (raised "Symbol.of_string: symbol must be uppercase") |}]
;;
