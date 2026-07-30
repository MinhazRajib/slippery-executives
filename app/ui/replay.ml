(* Runs the simulation (and the immediate baseline) once, in the browser, and
   precomputes everything the playback screens read minute by minute. Pure
   OCaml — no Bonsai here. *)

open! Core
open! Execlab_types
open! Execlab_market
open! Execlab_execution
open! Execlab_simulation
open! Execlab_analytics

type parent_replay =
  { instruction : Alpha_instruction.t
  ; order_ids : Order_id.Set.t
  }

type event =
  { time : Time_ns.Ofday.t
  ; line : string
  }

type result_row =
  { side : Side.t
  ; quantity : int
  ; filled_pct : float
  ; avg_fill : string
  ; shortfall_bps : string
  ; value_add_cents : int
  }

type results =
  { rows : result_row list
  ; total_value_add_cents : int
  }

type t =
  { algo_name : string
  ; bars : Market_bar.t array
  ; fills : Fill.t array
  ; parents : parent_replay list
  ; events : event array (* ascending by time *)
  ; results : results
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

let parents_of (result : Driver.t) =
  List.map (Order_manager.parents result.manager) ~f:(fun parent ->
    { instruction = parent.Parent_order.instruction
    ; order_ids =
        Order_id.Set.of_list
          (List.map parent.children ~f:(fun child -> child.Child_order.id))
    })
;;

let fills_of_parent (result : Driver.t) (parent : parent_replay) =
  List.filter result.fills ~f:(fun fill ->
    Set.mem parent.order_ids fill.Fill.order_id)
;;

let grade ~day (result : Driver.t) =
  let day_vwap = Day_stats.vwap day in
  let terminal_price = Benchmarks.terminal_price day in
  let half_spread = Fill_model.Config.default.half_spread in
  List.map (parents_of result) ~f:(fun parent ->
    let instruction = parent.instruction in
    let arrival_price =
      Or_error.ok_exn
        (Benchmarks.arrival_price
           day
           ~arrival_time:instruction.Alpha_instruction.arrival_time)
    in
    Or_error.ok_exn
      (Transaction_cost.create
         ~instruction
         ~fills:(fills_of_parent result parent)
         ~arrival_price
         ~terminal_price
         ~day_vwap
         ~half_spread))
;;

let results ~day ~algo_result ~baseline_result =
  let algo = grade ~day algo_result in
  let baseline = grade ~day baseline_result in
  let rows =
    List.map2_exn algo baseline ~f:(fun a b ->
      let value_add_cents =
        Or_error.ok_exn
          (Transaction_cost.value_add_cents ~algo:a ~baseline:b)
      in
      let avg_fill, shortfall_bps =
        match a.Transaction_cost.fill_metrics with
        | None -> "-", "-"
        | Some m ->
          ( sprintf "$%.2f" m.average_fill_price
          , sprintf "%+.1f" m.shortfall_bps )
      in
      { side = a.side
      ; quantity = Size.to_int a.quantity
      ; filled_pct = a.completion_rate *. 100.
      ; avg_fill
      ; shortfall_bps
      ; value_add_cents
      })
  in
  { rows
  ; total_value_add_cents =
      List.sum (module Int) rows ~f:(fun row -> row.value_add_cents)
  }
;;

let events ~(algo_result : Driver.t) parents =
  let side_str side =
    match (side : Side.t) with Buy -> "BUY" | Sell -> "SELL"
  in
  let activations =
    List.map parents ~f:(fun parent ->
      let i = parent.instruction in
      { time = i.Alpha_instruction.arrival_time
      ; line =
          sprintf
            "ACTIVATE %s %d TSLA (by %s)"
            (side_str i.side)
            (Size.to_int i.quantity)
            (String.prefix (Time_ns.Ofday.to_string i.deadline) 5)
      })
  in
  let fills =
    List.map algo_result.fills ~f:(fun fill ->
      { time = fill.Fill.time
      ; line =
          sprintf
            "FILL %s %d @ %s (%s)"
            (side_str fill.side)
            (Size.to_int fill.size)
            (Price.to_string_dollar fill.price)
            (match fill.liquidity with Taker -> "taker" | Maker -> "maker")
      })
  in
  let all =
    List.stable_sort (activations @ fills) ~compare:(fun a b ->
      Time_ns.Ofday.compare a.time b.time)
  in
  Array.of_list all
;;

let run ~algo_name =
  let day = Lazy.force day in
  let instructions = Lazy.force instructions in
  let run_one algorithm = Driver.run ~day ~instructions ~algorithm () in
  let algo_result = run_one (algorithm_named algo_name) in
  let baseline_result = run_one (module Immediate) in
  let parents = parents_of algo_result in
  { algo_name
  ; bars = Array.of_list day.Trading_day.bars
  ; fills = Array.of_list algo_result.fills
  ; parents
  ; events = events ~algo_result parents
  ; results = results ~day ~algo_result ~baseline_result
  }
;;

let last_minute t = Array.length t.bars - 1
let time_at t ~minute = t.bars.(minute).Market_bar.time

let clock_string t ~minute =
  String.prefix (Time_ns.Ofday.to_string (time_at t ~minute)) 5
;;

let fills_upto t ~minute =
  let cutoff = time_at t ~minute in
  Array.filter t.fills ~f:(fun fill ->
    Time_ns.Ofday.( <= ) fill.Fill.time cutoff)
  |> Array.to_list
;;

let events_upto t ~minute =
  let cutoff = time_at t ~minute in
  Array.filter t.events ~f:(fun event ->
    Time_ns.Ofday.( <= ) event.time cutoff)
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
