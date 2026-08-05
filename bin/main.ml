(** Execlab entry point.

    Parse-only: [dune exec bin/main.exe -- examples/demo_alpha.csv]

    Full run (algorithm vs. the immediate baseline on a real day):
    [dune exec bin/main.exe -- examples/demo_alpha_tsla.csv TSLA 2026-07-09 twap] *)

open! Core
open Execlab_types
open! Execlab_market
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

let run_report ~alpha_file ~date ~algo_name ~engine =
  let date = Date.of_string date in
  let instructions = (parse_alpha alpha_file).instructions in
  let symbols =
    Execlab_server.Catalog.symbols_of_instructions instructions
  in
  let universe =
    Or_error.ok_exn
      (Execlab_server.Catalog.universe ~data_dir:"data" ~date ~symbols)
  in
  let outcome =
    Or_error.ok_exn
      (Execlab_session.run
         ~universe
         ~forecast_days:
           (Execlab_server.Catalog.forecast_days_for
              ~data_dir:"data"
              ~date
              ~symbols)
         ~instructions
         ~algo_name
         ~params:{ Execlab_session.Params.default with engine })
  in
  printf
    "%s %s: %d instruction(s) across %s, %s vs immediate baseline\n\n"
    (String.concat ~sep:"," (List.map symbols ~f:Symbol.to_string))
    (Date.to_string date)
    (List.length instructions)
    (match List.length symbols with
     | 1 -> "one symbol"
     | n -> sprintf "%d symbols" n)
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

(* The alpha now names its own symbols, so a date is all the market a run
   needs. The older form with an explicit symbol still works and means what
   it always did: assert that the whole file trades that one name. *)
let run_report ~alpha_file ?only_symbol ~date ~algo_name ~engine () =
  match only_symbol with
  | None -> run_report ~alpha_file ~date ~algo_name ~engine
  | Some only ->
    let only = Symbol.of_string (String.uppercase only) in
    let instructions = (parse_alpha alpha_file).instructions in
    (match
       List.find instructions ~f:(fun instruction ->
         not (Symbol.equal instruction.Alpha_instruction.symbol only))
     with
     | None -> run_report ~alpha_file ~date ~algo_name ~engine
     | Some instruction ->
       (* A mismatch here is a typo in an argument, not a bug: report it the
          way a rejected alpha file is reported. *)
       print_s
         [%message
           "alpha names a symbol other than the one asked for"
             ~asked_for:(only : Symbol.t)
             (instruction : Alpha_instruction.t)
             ~hint:"drop the symbol argument to run every name it trades"];
       exit 1)
;;

let is_date text =
  Option.is_some (Option.try_with (fun () -> Date.of_string text))
;;

let () =
  let bar = Execlab_session.Engine_choice.Bar_model in
  match Sys.get_argv () with
  | [| _; filename |] -> print_instructions filename
  | [| _; alpha_file; date |] when is_date date ->
    run_report ~alpha_file ~date ~algo_name:"twap" ~engine:bar ()
  | [| _; alpha_file; date; algo_name |] when is_date date ->
    run_report ~alpha_file ~date ~algo_name ~engine:bar ()
  | [| _; alpha_file; date; algo_name; engine |] when is_date date ->
    run_report
      ~alpha_file
      ~date
      ~algo_name
      ~engine:(engine_of_string engine)
      ()
  | [| _; alpha_file; symbol; date |] ->
    run_report
      ~alpha_file
      ~only_symbol:symbol
      ~date
      ~algo_name:"twap"
      ~engine:bar
      ()
  | [| _; alpha_file; symbol; date; algo_name |] ->
    run_report
      ~alpha_file
      ~only_symbol:symbol
      ~date
      ~algo_name
      ~engine:bar
      ()
  | [| _; alpha_file; symbol; date; algo_name; engine |] ->
    run_report
      ~alpha_file
      ~only_symbol:symbol
      ~date
      ~algo_name
      ~engine:(engine_of_string engine)
      ()
  | _ ->
    eprintf
      "usage: main.exe <alpha.csv>                        (parse only)\n";
    eprintf
      "       main.exe <alpha.csv> <DATE> [twap|vwap|pov|is|immediate] \
       [bar|synthetic[:N]]\n";
    eprintf
      "       main.exe <alpha.csv> <SYMBOL> <DATE> ...    (asserts one \
       symbol)\n";
    exit 2
;;
