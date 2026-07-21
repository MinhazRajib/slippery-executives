(** Sandbox entry point.

    Run with: dune exec bin/main.exe -- 150.25 *)

open! Core
open Sandbox_types

let () =
  let price =
    if Array.length (Sys.get_argv ()) > 1
    then Price.of_string (Sys.get_argv ()).(1)
    else Price.of_int_cents 10000
  in
  print_endline (Price.to_string_dollar price)
;;
