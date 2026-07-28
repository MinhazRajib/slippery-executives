open! Core
open! Execlab_types

type t = { instructions : Alpha_instruction.t list }
[@@deriving sexp, bin_io, compare, equal]

(* take a line and parse it into alpha instruction *)
let parse_line_exn ~line_number line =
  match String.split line ~on:',' with
  | [ arrival_time; symbol; side; quantity; deadline ] ->
    let arrival_time = Time_ns.Ofday.of_string arrival_time in
    let symbol = Symbol.of_string symbol in
    let side =
      match String.uppercase side with
      | "BUY" -> Side.Buy
      | "SELL" -> Side.Sell
      | other ->
        failwith [%string "Line %{line_number#Int}: invalid side '%{other}'"]
    in
    let quantity = Size.of_string quantity in
    let deadline = Time_ns.Ofday.of_string deadline in
    Or_error.ok_exn
      (Alpha_instruction.create
         ~arrival_time
         ~symbol
         ~side
         ~quantity
         ~deadline)
  | _ -> failwith [%string "Line %{line_number#Int}: expected 5 fields"]
;;

(* take in csv file and turn it into a list of alpha instructions. *)
let parse contents =
  Or_error.try_with (fun () ->
    let lines = String.split_lines (String.strip contents) in
    let instructions =
      List.mapi lines ~f:(fun i line ->
        parse_line_exn ~line_number:(i + 1) line)
    in
    { instructions })
;;
