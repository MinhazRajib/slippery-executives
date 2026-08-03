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
  ; arrival_minute : int
  ; deadline_minute : int
  ; arrival_price : Price.t
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
  ; vwap_by_minute : float array
      (* running session VWAP (typical price, volume weighted), dollars *)
  ; target_by_minute : float array
      (* scheduled cumulative shares across all parents (linear ramps) *)
  ; actual_by_minute : int array (* cumulative filled shares *)
  ; cumulative_volume : int array (* market volume traded so far *)
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

(* VWAP's forecast: the average volume curve of the *other* embedded TSLA
   sessions — the simulated day is left out so the algorithm never peeks at
   the volume it is about to trade against. *)
let forecast_profile =
  lazy
    (let others =
       List.map Embedded_data.tsla_other_days ~f:(fun (date, csv) ->
         Or_error.ok_exn
           (Data_loader.parse
              ~symbol:(Symbol.of_string "TSLA")
              ~date:(Date.of_string date)
              csv))
     in
     Or_error.ok_exn (Day_stats.average_volume_profile others))
;;

let algorithm_named ~(day : Trading_day.t) = function
  | "immediate" -> (module Immediate : Algorithm_intf.S)
  | "vwap" ->
    let profile =
      List.map2_exn
        day.bars
        (Lazy.force forecast_profile)
        ~f:(fun bar weight -> bar.Market_bar.time, weight)
    in
    Vwap.create ~profile
  (* Sized for the demo orders (~0.1% of their windows' volume) so the
     tape-chasing is visible; a street-realistic 5-20% would demand the whole
     parent off the first observed bar. *)
  | "pov" -> Pov.create ~participation_rate:0.0015 ()
  | _ -> (module Twap : Algorithm_intf.S)
;;

let minute_of_ofday ~(bars : Market_bar.t array) time =
  Float.to_int
    (Time_ns.Span.to_min (Time_ns.Ofday.diff time bars.(0).Market_bar.time))
;;

let parents_of ~bars (result : Driver.t) =
  List.map (Order_manager.parents result.manager) ~f:(fun parent ->
    let instruction = parent.Parent_order.instruction in
    let arrival_minute =
      minute_of_ofday ~bars instruction.Alpha_instruction.arrival_time
    in
    { instruction
    ; order_ids =
        Order_id.Set.of_list
          (List.map parent.children ~f:(fun child -> child.Child_order.id))
    ; arrival_minute
    ; deadline_minute =
        minute_of_ofday ~bars instruction.Alpha_instruction.deadline
    ; arrival_price = bars.(arrival_minute).Market_bar.open_
    })
;;

let fills_of_parent (result : Driver.t) (parent : parent_replay) =
  List.filter result.fills ~f:(fun fill ->
    Set.mem parent.order_ids fill.Fill.order_id)
;;

let grade ~day (result : Driver.t) ~parents =
  let day_vwap = Day_stats.vwap day in
  let terminal_price = Benchmarks.terminal_price day in
  let half_spread = Fill_model.Config.default.half_spread in
  List.map parents ~f:(fun parent ->
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

let results ~day ~bars ~algo_result ~baseline_result =
  let algo =
    grade ~day algo_result ~parents:(parents_of ~bars algo_result)
  in
  let baseline =
    grade ~day baseline_result ~parents:(parents_of ~bars baseline_result)
  in
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
  List.stable_sort (activations @ fills) ~compare:(fun a b ->
    Time_ns.Ofday.compare a.time b.time)
  |> Array.of_list
;;

let vwap_by_minute bars =
  let dollar_volume = ref 0. in
  let volume = ref 0. in
  Array.map bars ~f:(fun (bar : Market_bar.t) ->
    let typical =
      (Price.to_float bar.high
       +. Price.to_float bar.low
       +. Price.to_float bar.close)
      /. 3.
    in
    let v = Float.of_int (Size.to_int bar.volume) in
    dollar_volume := !dollar_volume +. (typical *. v);
    volume := !volume +. v;
    if Float.( > ) !volume 0. then !dollar_volume /. !volume else typical)
;;

(* The scheduled cumulative quantity at each minute: every parent ramps
   linearly from activation to deadline (the TWAP ideal; also the honest
   reference schedule for any algorithm). *)
let target_by_minute ~bars parents =
  Array.init (Array.length bars) ~f:(fun m ->
    List.sum (module Float) parents ~f:(fun parent ->
      let total =
        Float.of_int
          (Size.to_int parent.instruction.Alpha_instruction.quantity)
      in
      if m < parent.arrival_minute
      then 0.
      else if m >= parent.deadline_minute
      then total
      else (
        let span = parent.deadline_minute - parent.arrival_minute in
        total
        *. Float.of_int (m - parent.arrival_minute)
        /. Float.of_int span)))
;;

let actual_by_minute ~bars (fills : Fill.t array) =
  let per_minute = Array.create ~len:(Array.length bars) 0 in
  Array.iter fills ~f:(fun fill ->
    let m = minute_of_ofday ~bars fill.time in
    per_minute.(m) <- per_minute.(m) + Size.to_int fill.size);
  let total = ref 0 in
  Array.map per_minute ~f:(fun v ->
    total := !total + v;
    !total)
;;

let cumulative_volume bars =
  let total = ref 0 in
  Array.map bars ~f:(fun (bar : Market_bar.t) ->
    total := !total + Size.to_int bar.volume;
    !total)
;;

let run ~algo_name =
  let day = Lazy.force day in
  let instructions = Lazy.force instructions in
  let run_one algorithm = Driver.run ~day ~instructions ~algorithm () in
  let algo_result = run_one (algorithm_named ~day algo_name) in
  let baseline_result = run_one (module Immediate) in
  let bars = Array.of_list day.Trading_day.bars in
  let parents = parents_of ~bars algo_result in
  let fills = Array.of_list algo_result.fills in
  { algo_name
  ; bars
  ; fills
  ; parents
  ; events = events ~algo_result parents
  ; results = results ~day ~bars ~algo_result ~baseline_result
  ; vwap_by_minute = vwap_by_minute bars
  ; target_by_minute = target_by_minute ~bars parents
  ; actual_by_minute = actual_by_minute ~bars fills
  ; cumulative_volume = cumulative_volume bars
  }
;;

(* Mark-to-market P&L of everything executed so far, against [last]: longs
   gain as the tape rises, shorts the reverse. *)
let open_pnl_cents ~(fills : Fill.t list) ~last =
  List.sum (module Int) fills ~f:(fun fill ->
    Side.sign fill.side
    * (Price.to_int_cents last - Price.to_int_cents fill.price)
    * Size.to_int fill.size)
;;

let last_minute t = Array.length t.bars - 1
let time_at t ~minute = t.bars.(minute).Market_bar.time
let minute_of_time t time = minute_of_ofday ~bars:t.bars time

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

(* Percent ahead (+) or behind (-) the scheduled quantity; None before
   anything is scheduled. *)
let schedule_delta_pct t ~minute =
  let target = t.target_by_minute.(minute) in
  if Float.( <= ) target 1.
  then None
  else (
    let actual = Float.of_int t.actual_by_minute.(minute) in
    Some ((actual -. target) /. target *. 100.))
;;

type parent_row =
  { id : string
  ; side : Side.t
  ; total : int
  ; filled : int
  ; window : string
  ; avg_fill : string
  ; status : string
  }

let parent_rows t ~minute =
  let now = time_at t ~minute in
  let fills = fills_upto t ~minute in
  List.mapi t.parents ~f:(fun index parent ->
    let instruction = parent.instruction in
    let total = Size.to_int instruction.Alpha_instruction.quantity in
    let mine =
      List.filter fills ~f:(fun fill ->
        Set.mem parent.order_ids fill.Fill.order_id)
    in
    let filled =
      List.sum (module Int) mine ~f:(fun fill -> Size.to_int fill.size)
    in
    let avg_fill =
      if filled = 0
      then "-"
      else (
        let notional =
          List.sum (module Int) mine ~f:(fun fill ->
            Fill.notional_cents fill)
        in
        sprintf "$%.2f" (notional // filled /. 100.))
    in
    let status =
      if Time_ns.Ofday.( < ) now instruction.Alpha_instruction.arrival_time
      then "PENDING"
      else if filled >= total
      then "DONE"
      else if Time_ns.Ofday.( > ) now instruction.Alpha_instruction.deadline
      then "EXPIRED"
      else "WORKING"
    in
    let window =
      sprintf
        "%s-%s"
        (String.prefix
           (Time_ns.Ofday.to_string
              instruction.Alpha_instruction.arrival_time)
           5)
        (String.prefix
           (Time_ns.Ofday.to_string instruction.Alpha_instruction.deadline)
           5)
    in
    { id = sprintf "P-%d" (index + 1)
    ; side = instruction.Alpha_instruction.side
    ; total
    ; filled
    ; window
    ; avg_fill
    ; status
    })
;;

(* Which parent (by position) owns this order id; used to color blotter lines
   and fill ticks by order. *)
let parent_index_of_order t order_id =
  List.findi t.parents ~f:(fun (_ : int) parent ->
    Set.mem parent.order_ids order_id)
  |> Option.map ~f:fst
  |> Option.value ~default:0
;;
