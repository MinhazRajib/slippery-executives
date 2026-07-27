(** Execlab entry point: parse an alpha-output CSV file and print the
    validated trade instructions.

    Run with: dune exec bin/main.exe -- examples/demo_alpha.csv *)

open! Core
open Execlab_types

let () =
  match Sys.get_argv () with
  | [| _; filename |] ->
    let contents = In_channel.read_all filename in
    (match Execlab_alpha.Parser.parse contents with
     | Ok (parsed : Execlab_alpha.Parser.t) ->
       printf
         "Parsed %d instructions from %s:\n\n"
         (List.length parsed.instructions)
         filename;
       List.iter parsed.instructions ~f:(fun instruction ->
         print_s [%sexp (instruction : Alpha_instruction.t)])
     | Error error ->
       print_s
         [%message
           "Rejected alpha file" (filename : string) (error : Error.t)];
       exit 1)
  | _ ->
    eprintf "usage: main.exe <alpha_output.csv>\n";
    exit 2
;;
