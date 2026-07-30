(* Runs the simulation once (in the browser) and precomputes everything the
   playback screens read minute by minute. Pure OCaml — no Bonsai here. *)

open! Core
open! Execlab_types
open! Execlab_market
open! Execlab_execution
open! Execlab_simulation

type parent_replay =
  { instruction : Alpha_instruction.t
  ; order_ids : Order_id.Set.t
  ; final_status : Parent_order.Status.t
  }

type t =
  { algo_name : string
  ; bars : Market_bar.t array
  ; fills : Fill.t array (* in fill order *)
  ; parents : parent_replay list
  }

let day =
  lazy
    (Or_error.ok_exn
       (Data_loader.parse
          ~symbol:(Symbol.of_string "TSLA")
          ~date:(Date.of_string "2026-07-09")
          Embedded_data.tsla_day))
;;

let instructions =
  lazy
    (Or_error.ok_exn (Execlab_alpha.Parser.parse Embedded_data.demo_alpha))
      .instructions
;;

let demo_instructions () = Lazy.force instructions

let algorithm_named = function
  | "immediate" -> (module Immediate : Algorithm_intf.S)
  | _ -> (module Twap : Algorithm_intf.S)
;;

let run ~algo_name =
  let day = Lazy.force day in
  let instructions = Lazy.force instructions in
  let result =
    Driver.run ~day ~instructions ~algorithm:(algorithm_named algo_name) ()
  in
  let parents =
    List.map (Order_manager.parents result.manager) ~f:(fun parent ->
      { instruction = parent.Parent_order.instruction
      ; order_ids =
          Order_id.Set.of_list
            (List.map parent.children ~f:(fun child -> child.Child_order.id))
      ; final_status = parent.status
      })
  in
  { algo_name
  ; bars = Array.of_list day.Trading_day.bars
  ; fills = Array.of_list result.fills
  ; parents
  }
;;

let last_minute t = Array.length t.bars - 1
let time_at t ~minute = t.bars.(minute).Market_bar.time

(* "HH:MM" *)
let clock_string t ~minute =
  String.prefix (Time_ns.Ofday.to_string (time_at t ~minute)) 5
;;

let fills_upto t ~minute =
  let cutoff = time_at t ~minute in
  Array.filter t.fills ~f:(fun fill ->
    Time_ns.Ofday.( <= ) fill.Fill.time cutoff)
  |> Array.to_list
;;

let filled_shares fills ~order_ids =
  List.sum (module Int) fills ~f:(fun (fill : Fill.t) ->
    if Set.mem order_ids fill.order_id then Size.to_int fill.size else 0)
;;

type parent_row =
  { side : Side.t
  ; total : int
  ; filled : int
  ; status : string
  }

let parent_rows t ~minute =
  let now = time_at t ~minute in
  let fills = fills_upto t ~minute in
  List.map t.parents ~f:(fun parent ->
    let instruction = parent.instruction in
    let total = Size.to_int instruction.Alpha_instruction.quantity in
    let filled = filled_shares fills ~order_ids:parent.order_ids in
    let status =
      if Time_ns.Ofday.( < ) now instruction.Alpha_instruction.arrival_time
      then "PENDING"
      else if filled >= total
      then "DONE"
      else if Time_ns.Ofday.( > ) now instruction.Alpha_instruction.deadline
      then "EXPIRED"
      else "WORKING"
    in
    { side = instruction.Alpha_instruction.side; total; filled; status })
;;
