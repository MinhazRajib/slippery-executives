open! Core
open! Execlab_types

type t = { instructions : list Alpha_instruction.t }
[@@deriving sexp, bin_io, compare, equal]

(*take in csv file and get an alpha instruction*)


let parse_line ~line_number line = 
  let result =
    match String.split line ~on:',' with
    | [ arrival_time; symbol; side; quantity; deadline ] ->
      let arrival_time = Time_ns.Ofday.of_string arrival_time in
      let symbol = Symbol.of_string symbol in
      let side = 
        match String.uppercase side with
        | "BUY" -> Side.Buy
        | "SELL" -> Side.Sell
        | other -> failwith (Printf.sprintf "Line %d: Invalid side '%s'" line_number other)
      in
      let quantity = Int.of_string quantity in
      let deadline = Time_ns.Ofday.of_string deadline in



      match Alpha_instruction.create ~arrival_time ~symbol ~side ~quantity ~deadline with
      | Ok instruction -> Ok instruction
      | Error msg -> Error (Printf.sprintf "Line %d: %s" line_number msg)
    | _ ->
      Error (Printf.sprintf "Line %d: Invalid number of fields" line_number)


    ;;




  let parse contents =
  let lines = String.split_lines (String.strip contents) in
  let results = 
    List.mapi lines ~f:(fun line_number line ->
      parse_line ~line_number:(line_number + 1) line)
  in
  match Result.all results with
  | Ok instructions -> Ok { instructions }
  | Error msg -> Error msg 

;;

