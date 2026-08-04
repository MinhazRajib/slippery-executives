open! Core
open! Execlab_types
open Execlab_protocol

let%expect_test "alpha hash ignores cosmetic whitespace, not content" =
  let base = "10:00:00,TSLA,BUY,5000,11:00:00\n" in
  let padded = "  10:00:00,TSLA,BUY,5000,11:00:00  \n\n" in
  let changed = "10:00:00,TSLA,BUY,5001,11:00:00\n" in
  printf
    "cosmetic: %b\n"
    (String.equal (alpha_hash base) (alpha_hash padded));
  printf
    "content:  %b\n"
    (String.equal (alpha_hash base) (alpha_hash changed));
  [%expect {|
    cosmetic: true
    content:  false
    |}]
;;

let%expect_test "run config round-trips through its sexp" =
  let config =
    { Run_config.player = "qasim"
    ; symbol = Symbol.of_string "TSLA"
    ; date = Date.of_string "2026-07-09"
    ; alpha_text = "10:00:00,TSLA,BUY,5000,11:00:00\n"
    ; algo_name = "is"
    ; half_spread_cents = 2
    ; max_participation = 0.1
    ; impact_coefficient_cents = 25
    ; pov_rate = 0.0015
    ; is_urgency = 2.0
    ; engine_name = "synthetic"
    ; engine_seed = 7
    }
  in
  let round_tripped = Run_config.t_of_sexp (Run_config.sexp_of_t config) in
  printf "round-trips: %b\n" (Run_config.equal config round_tripped);
  [%expect {| round-trips: true |}]
;;
