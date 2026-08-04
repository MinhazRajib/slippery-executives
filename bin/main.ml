(** Execlab entry point.

    Parse-only: [dune exec bin/main.exe -- examples/demo_alpha.csv]

    Full run (algorithm vs. the immediate baseline on a real day):
    [dune exec bin/main.exe -- examples/demo_alpha_tsla.csv TSLA 2026-07-09 twap] *)

open! Core
open Execlab_types
open Execlab_market
open Execlab_execution
open Execlab_simulation
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

(* POV's default rate is sized for the demo instructions, which are tiny next
   to a liquid tape (~0.1% of their windows' volume): at a street- realistic
   5-20% the whole parent is demanded off the first observed bar and fills in
   a minute. 0.15% keeps the tape-chasing visible. *)
let default_pov_rate = 0.0015

(* Front-loads noticeably without slamming the tape; see the mli. *)
let default_is_urgency = 2.0

(* VWAP's forecast: the average volume curve of every *other* session we have
   for the symbol, so the algorithm never sees the day it is about to trade.
   With no other sessions on disk, fall back to the day's own curve (the old,
   peeking behavior — better than refusing to run). *)
let forecast_profile ~symbol ~date ~(day : Trading_day.t) =
  let other_days =
    Sys_unix.readdir ("data" ^/ Symbol.to_string symbol)
    |> Array.to_list
    |> List.filter_map ~f:(fun file ->
      let open Option.Let_syntax in
      let%bind date_string = String.chop_suffix file ~suffix:".csv" in
      let%bind other =
        Option.try_with (fun () -> Date.of_string date_string)
      in
      if Date.equal other date
      then None
      else Some (Or_error.ok_exn (Data_loader.load ~symbol ~date:other ())))
  in
  let profile =
    match Day_stats.average_volume_profile other_days with
    | Ok profile -> profile
    | Error (_ : Error.t) -> Day_stats.volume_profile day
  in
  List.map2_exn day.bars profile ~f:(fun bar weight ->
    bar.Market_bar.time, weight)
;;

let algorithm_named ~symbol ~date ~(day : Trading_day.t) = function
  | "twap" -> (module Twap : Algorithm_intf.S)
  | "immediate" -> (module Immediate : Algorithm_intf.S)
  | "vwap" -> Vwap.create ~profile:(forecast_profile ~symbol ~date ~day)
  | "pov" -> Pov.create ~participation_rate:default_pov_rate ()
  | "is" -> Implementation_shortfall.create ~urgency:default_is_urgency ()
  | other ->
    raise_s
      [%message
        "Unknown algorithm"
          (other : string)
          ~known:"twap, vwap, pov, is, immediate"]
;;

let fills_for_parent (result : Driver.t) (parent : Parent_order.t) =
  let ids =
    Order_id.Set.of_list
      (List.map parent.children ~f:(fun child -> child.Child_order.id))
  in
  List.filter result.fills ~f:(fun fill -> Set.mem ids fill.order_id)
;;

(* One grading per instruction, in instruction order. *)
let grade ~day (result : Driver.t) =
  let day_vwap = Day_stats.vwap day in
  let terminal_price = Benchmarks.terminal_price day in
  let half_spread = Fill_model.Config.default.half_spread in
  List.map (Order_manager.parents result.manager) ~f:(fun parent ->
    let instruction = parent.Parent_order.instruction in
    let arrival_price =
      Or_error.ok_exn
        (Benchmarks.arrival_price
           day
           ~arrival_time:instruction.Alpha_instruction.arrival_time)
    in
    Or_error.ok_exn
      (Transaction_cost.create
         ~instruction
         ~fills:(fills_for_parent result parent)
         ~day
         ~arrival_price
         ~terminal_price
         ~day_vwap
         ~half_spread))
;;

let run_report ~alpha_file ~symbol ~date ~algo_name =
  let symbol = Symbol.of_string (String.uppercase symbol) in
  let date = Date.of_string date in
  let day = Or_error.ok_exn (Data_loader.load ~symbol ~date ()) in
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
  let run algorithm = Driver.run ~day ~instructions ~algorithm () in
  let algo_gradings =
    grade ~day (run (algorithm_named ~symbol ~date ~day algo_name))
  in
  let baseline_gradings = grade ~day (run (module Immediate)) in
  printf
    "%s %s: %d instruction(s), %s vs immediate baseline\n\n"
    (Symbol.to_string symbol)
    (Date.to_string date)
    (List.length instructions)
    algo_name;
  List.iter2_exn algo_gradings baseline_gradings ~f:(fun algo baseline ->
    print_endline
      (Or_error.ok_exn
         (Report.comparison
            ~algo
            ~algo_name
            ~baseline
            ~baseline_name:"immediate"));
    print_endline "")
;;

let () =
  match Sys.get_argv () with
  | [| _; filename |] -> print_instructions filename
  | [| _; alpha_file; symbol; date |] ->
    run_report ~alpha_file ~symbol ~date ~algo_name:"twap"
  | [| _; alpha_file; symbol; date; algo_name |] ->
    run_report ~alpha_file ~symbol ~date ~algo_name
  | _ ->
    eprintf
      "usage: main.exe <alpha.csv>                        (parse only)\n";
    eprintf
      "       main.exe <alpha.csv> <SYMBOL> <DATE> \
       [twap|vwap|pov|is|immediate]\n";
    exit 2
;;
