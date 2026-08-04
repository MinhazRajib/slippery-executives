open! Core
open! Execlab_types

type t = { instructions : Alpha_instruction.t list }
[@@deriving sexp, bin_io, compare, equal]

let parse_line ~line_number line =
  Or_error.tag_s
    (Or_error.try_with (fun () ->
       match List.map (String.split line ~on:',') ~f:String.strip with
       | [ arrival_time; symbol; side; quantity; deadline ] ->
         let side =
           match String.uppercase side with
           | "BUY" -> Side.Buy
           | "SELL" -> Side.Sell
           | other -> raise_s [%message "invalid side" (other : string)]
         in
         Or_error.ok_exn
           (Alpha_instruction.create
              ~arrival_time:(Time_ns.Ofday.of_string arrival_time)
              ~symbol:(Symbol.of_string symbol)
              ~side
              ~quantity:(Size.of_string quantity)
              ~deadline:(Time_ns.Ofday.of_string deadline))
       | fields ->
         raise_s
           [%message
             "expected 5 comma-separated fields"
               ~got:(List.length fields : int)]))
    ~tag:[%message "Invalid instruction" (line_number : int) (line : string)]
;;

(* Alpha files are written by other people's tooling, so the shapes that
   arrive are not always the shape we asked for: a header row from a
   spreadsheet export, blank lines between blocks, padding around the commas.
   A row that starts with something other than a time of day is the header —
   a real instruction always starts with its arrival. *)
let is_header line =
  match String.split line ~on:',' with
  | [] -> false
  | first :: (_ : string list) ->
    Option.is_none
      (Option.try_with (fun () ->
         Time_ns.Ofday.of_string (String.strip first)))
;;

let parse contents =
  let numbered =
    String.split_lines contents
    |> List.mapi ~f:(fun i line -> i + 1, line)
    |> List.filter ~f:(fun ((_ : int), line) ->
      not (String.is_empty (String.strip line)))
  in
  let rows =
    match numbered with
    | (_ : int * string) :: rest when is_header (snd (List.hd_exn numbered))
      ->
      rest
    | (_ : (int * string) list) -> numbered
  in
  let open Or_error.Let_syntax in
  let%map instructions =
    List.map rows ~f:(fun (line_number, line) ->
      parse_line ~line_number line)
    |> Or_error.combine_errors
  in
  { instructions }
;;
