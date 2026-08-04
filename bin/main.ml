(** Execlab entry point.

    Parse-only: [dune exec bin/main.exe -- examples/demo_alpha.csv]

    Full run (algorithm vs. the immediate baseline on a real day):
    [dune exec bin/main.exe -- examples/demo_alpha_tsla.csv TSLA 2026-07-09 twap] *)

open! Core
open Execlab_types
open Execlab_market
open Execlab_analytics

let parse_alpha filename : Execlab_alpha.Parser.t =
  match Execlab_alpha.Parser.parse (In_channel.read_all filename) with
  | Ok parsed -> parsed
  | Error error ->
    print_s
      [%message "Rejected alpha file" (filename : string) (error : Error.t)];
    exit 1
;;

let print_instructions filename =
  let parsed = parse_alpha filename in
  printf
    "Parsed %d instructions from %s:\n\n"
    (List.length parsed.instructions)
    filename;
  List.iter parsed.instructions ~f:(fun instruction ->
    print_s [%sexp (instruction : Alpha_instruction.t)])
;;

(* "bar" (default) or "synthetic[:seed]". *)
let engine_of_string text =
  match String.lsplit2 text ~on:':' with
  | None when String.equal text "bar" ->
    Execlab_session.Engine_choice.Bar_model
  | None when String.equal text "synthetic" ->
    Execlab_session.Engine_choice.Synthetic { seed = 1 }
  | Some ("synthetic", seed) ->
    Execlab_session.Engine_choice.Synthetic { seed = Int.of_string seed }
  | Some ((_ : string), (_ : string)) | None ->
    raise_s
      [%message "unknown engine" (text : string) ~known:"bar, synthetic[:N]"]
;;

let run_report ~alpha_file ~symbol ~date ~algo_name ~engine =
  let symbol = Symbol.of_string (String.uppercase symbol) in
  let date = Date.of_string date in
  let day = Or_error.ok_exn (Data_loader.load ~symbol ~date ()) in
  let forecast_days =
    Execlab_server.Catalog.forecast_days
      ~data_dir:"data"
      ~symbol
      ~excluding:date
  in
  let instructions = (parse_alpha alpha_file).instructions in
  (match
     List.find instructions ~f:(fun instruction ->
       not (Symbol.equal instruction.Alpha_instruction.symbol symbol))
   with
   | None -> ()
   | Some instruction ->
     raise_s
       [%message
         "Instruction symbol does not match the loaded day"
           (instruction : Alpha_instruction.t)
           (symbol : Symbol.t)]);
  let outcome =
    Or_error.ok_exn
      (Execlab_session.run
         ~day
         ~forecast_days
         ~instructions
         ~algo_name
         ~params:{ Execlab_session.Params.default with engine })
  in
  printf
    "%s %s: %d instruction(s), %s vs immediate baseline\n\n"
    (Symbol.to_string symbol)
    (Date.to_string date)
    (List.length instructions)
    algo_name;
  List.iter outcome.graded ~f:(fun graded ->
    print_endline
      (Or_error.ok_exn
         (Report.comparison
            ~algo:graded.Execlab_session.Graded.grading
            ~algo_name
            ~baseline:graded.baseline
            ~baseline_name:"immediate"));
    print_endline "")
;;

let () =
  match Sys.get_argv () with
  | [| _; filename |] -> print_instructions filename
  | [| _; alpha_file; symbol; date |] ->
    run_report
      ~alpha_file
      ~symbol
      ~date
      ~algo_name:"twap"
      ~engine:Execlab_session.Engine_choice.Bar_model
  | [| _; alpha_file; symbol; date; algo_name |] ->
    run_report
      ~alpha_file
      ~symbol
      ~date
      ~algo_name
      ~engine:Execlab_session.Engine_choice.Bar_model
  | [| _; alpha_file; symbol; date; algo_name; engine |] ->
    run_report
      ~alpha_file
      ~symbol
      ~date
      ~algo_name
      ~engine:(engine_of_string engine)
  | _ ->
    eprintf
      "usage: main.exe <alpha.csv>                        (parse only)\n";
    eprintf
      "       main.exe <alpha.csv> <SYMBOL> <DATE> \
       [twap|vwap|pov|is|immediate] [bar|synthetic[:N]]\n";
    exit 2
;;
