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
  { grading : Transaction_cost.t
      (* the full metric tree: shortfall and its timing/spread/impact split,
         opportunity, gross/net, alpha capture *)
  ; value_add_cents : int (* vs the immediate baseline *)
  }

type results =
  { rows : result_row list
  ; total_value_add_cents : int
  }

type t =
  { symbol : Symbol.t
  ; date : Date.t
  ; algo_name : string
  ; alpha_text : string
  ; params : Execlab_session.Params.t
      (* what this run actually used — the submit-to-leaderboard config must
         reproduce the run exactly *)
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

let parse_alpha text =
  Or_error.map (Execlab_alpha.Parser.parse text) ~f:(fun parsed ->
    parsed.instructions)
;;

module Params = Execlab_session.Params

(* The setup screen's raw text fields, one per knob; parsed and validated
   only when the user hits Run. *)
module Param_text = struct
  type t =
    { half_spread : string (** dollars, e.g. ["0.02"] *)
    ; participation : string (** fraction of bar volume, e.g. ["0.10"] *)
    ; impact : string (** dollars at 100% participation *)
    ; pov_rate : string (** fraction of tape volume *)
    ; urgency : string (** IS front-loading, [0.] = TWAP *)
    ; engine : string (** ["bar"] or ["synthetic"] *)
    ; seed : string (** the synthetic engine's seed *)
    }
  [@@deriving sexp, equal]

  let default =
    let config = Fill_model.Config.default in
    { half_spread = sprintf "%.2f" (Price.to_float config.half_spread)
    ; participation = sprintf "%.2f" config.max_participation
    ; impact = sprintf "%.2f" (Price.to_float config.impact_coefficient)
    ; pov_rate = sprintf "%.4f" Params.default.pov_rate
    ; urgency = sprintf "%.1f" Params.default.is_urgency
    ; engine = "bar"
    ; seed = "1"
    }
  ;;
end

let parse_params (text : Param_text.t) : Params.t Or_error.t =
  let open Or_error.Let_syntax in
  let field name raw ~check ~message =
    match Float.of_string_opt (String.strip raw) with
    | Some value when check value -> Ok value
    | Some (_ : float) | None ->
      Or_error.error_s
        [%message "bad parameter" ~_:(name : string) (raw : string) message]
  in
  let dollars value =
    Price.of_int_cents (Float.iround_nearest_exn (value *. 100.))
  in
  let%bind half_spread =
    field
      "half spread"
      text.half_spread
      ~check:(fun v -> Float.( >= ) v 0. && Float.( < ) v 100.)
      ~message:"dollars, at least 0"
  in
  let%bind participation =
    field
      "participation cap"
      text.participation
      ~check:(fun v -> Float.( > ) v 0. && Float.( <= ) v 1.)
      ~message:"a fraction in (0, 1]"
  in
  let%bind impact =
    field
      "impact coefficient"
      text.impact
      ~check:(fun v -> Float.( >= ) v 0. && Float.( < ) v 100.)
      ~message:"dollars at full participation, at least 0"
  in
  let%bind pov_rate =
    field
      "pov rate"
      text.pov_rate
      ~check:(fun v -> Float.( > ) v 0. && Float.( <= ) v 1.)
      ~message:"a fraction in (0, 1]"
  in
  let%bind is_urgency =
    field
      "urgency"
      text.urgency
      ~check:(fun v -> Float.( >= ) v 0. && Float.( <= ) v 10_000.)
      ~message:"at least 0 (0 = TWAP)"
  in
  let%map engine =
    match text.engine with
    | "bar" -> Ok Execlab_session.Engine_choice.Bar_model
    | "synthetic" ->
      (match Int.of_string_opt (String.strip text.seed) with
       | Some seed -> Ok (Execlab_session.Engine_choice.Synthetic { seed })
       | None ->
         Or_error.error_s
           [%message
             "bad parameter" "seed" ~raw:(text.seed : string) "an integer"])
    | other ->
      Or_error.error_s
        [%message "unknown engine" (other : string) ~known:"bar, synthetic"]
  in
  { Params.fill_config =
      { half_spread = dollars half_spread
      ; max_participation = participation
      ; impact_coefficient = dollars impact
      }
  ; pov_rate
  ; is_urgency
  ; engine
  }
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

let events ~symbol ~(algo_result : Driver.t) parents =
  let side_str side =
    match (side : Side.t) with Buy -> "BUY" | Sell -> "SELL"
  in
  let activations =
    List.map parents ~f:(fun parent ->
      let i = parent.instruction in
      { time = i.Alpha_instruction.arrival_time
      ; line =
          sprintf
            "ACTIVATE %s %d %s (by %s)"
            (side_str i.side)
            (Size.to_int i.quantity)
            (Symbol.to_string symbol)
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

(* Alpha text and day selection are user input, so the whole run is an
   [Or_error]: a bad CSV or an instruction outside the chosen day comes back
   as a message for the setup screen, not an exception. *)
let run ~symbol ~date ~alpha_text ~algo_name ~(params : Params.t) =
  let open Or_error.Let_syntax in
  let%bind day = Dataset.load ~symbol ~date in
  let%bind instructions = parse_alpha alpha_text in
  let%bind () =
    if List.is_empty instructions
    then Or_error.error_string "the alpha has no instructions"
    else Ok ()
  in
  let forecast_days =
    Dataset.dates_for symbol
    |> List.filter ~f:(fun other -> not (Date.equal other date))
    |> List.filter_map ~f:(fun other ->
      Or_error.ok (Dataset.load ~symbol ~date:other))
  in
  let%map outcome =
    Execlab_session.run ~day ~forecast_days ~instructions ~algo_name ~params
  in
  let algo_result = outcome.Execlab_session.Outcome.algo_result in
  let bars = Array.of_list day.Trading_day.bars in
  let parents = parents_of ~bars algo_result in
  let fills = Array.of_list algo_result.fills in
  let rows =
    List.map outcome.graded ~f:(fun graded ->
      { grading = graded.Execlab_session.Graded.grading
      ; value_add_cents = graded.value_add_cents
      })
  in
  { symbol
  ; date
  ; algo_name
  ; alpha_text
  ; params
  ; bars
  ; fills
  ; parents
  ; events = events ~symbol ~algo_result parents
  ; results =
      { rows
      ; total_value_add_cents =
          Execlab_session.Outcome.value_add_cents outcome
      }
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

(* ---------- CSV export ---------- *)

(* One row per order: identity, window, benchmarks, fill quality, and the
   full cost tree. Costs are exact integer cents; prices are dollars. *)
let results_csv t =
  let header =
    "order,side,symbol,date,algo,quantity,filled,completion_rate,arrival_time,deadline,arrival_price,terminal_price,day_vwap,avg_fill_price,shortfall_bps,vwap_slippage_bps,timing_cost_cents,spread_cost_cents,impact_cost_cents,friction_cost_cents,opportunity_cost_cents,gross_pnl_cents,net_pnl_cents,value_add_vs_immediate_cents,alpha_capture"
  in
  let rows =
    List.mapi
      (List.zip_exn t.parents t.results.rows)
      ~f:(fun index (parent, row) ->
        let grading = row.grading in
        let instruction = parent.instruction in
        let fill_metric render =
          match grading.Transaction_cost.fill_metrics with
          | None -> ""
          | Some metrics -> render metrics
        in
        String.concat
          ~sep:","
          [ Int.to_string (index + 1)
          ; (match grading.side with Buy -> "BUY" | Sell -> "SELL")
          ; Symbol.to_string t.symbol
          ; Date.to_string t.date
          ; t.algo_name
          ; Int.to_string (Size.to_int grading.quantity)
          ; Int.to_string (Size.to_int grading.filled)
          ; sprintf "%.6f" grading.completion_rate
          ; String.prefix
              (Time_ns.Ofday.to_string
                 instruction.Alpha_instruction.arrival_time)
              8
          ; String.prefix
              (Time_ns.Ofday.to_string
                 instruction.Alpha_instruction.deadline)
              8
          ; sprintf "%.2f" (Price.to_float grading.arrival_price)
          ; sprintf "%.2f" (Price.to_float grading.terminal_price)
          ; sprintf "%.4f" grading.day_vwap
          ; fill_metric (fun metrics ->
              sprintf
                "%.6f"
                metrics.Transaction_cost.Fill_metrics.average_fill_price)
          ; fill_metric (fun metrics -> sprintf "%.4f" metrics.shortfall_bps)
          ; fill_metric (fun metrics ->
              sprintf "%.4f" metrics.vwap_slippage_bps)
          ; Int.to_string grading.timing_cost_cents
          ; Int.to_string grading.spread_cost_cents
          ; Int.to_string grading.impact_cost_cents
          ; Int.to_string grading.friction_cost_cents
          ; Int.to_string grading.opportunity_cost_cents
          ; Int.to_string grading.gross_theoretical_pnl_cents
          ; Int.to_string grading.net_pnl_cents
          ; Int.to_string row.value_add_cents
          ; (match grading.alpha_capture with
             | None -> ""
             | Some capture -> sprintf "%.6f" capture)
          ])
  in
  String.concat ~sep:"\n" (header :: rows) ^ "\n"
;;

(* Every fill of the graded run — the raw material behind the per-order
   numbers, tagged with its owning order. *)
let fills_csv t =
  let header = "fill_id,time,order,side,size,price,liquidity" in
  let rows =
    Array.to_list t.fills
    |> List.map ~f:(fun (fill : Fill.t) ->
      String.concat
        ~sep:","
        [ Int.to_string fill.fill_id
        ; String.prefix (Time_ns.Ofday.to_string fill.time) 8
        ; Int.to_string (parent_index_of_order t fill.order_id + 1)
        ; (match fill.side with Buy -> "BUY" | Sell -> "SELL")
        ; Int.to_string (Size.to_int fill.size)
        ; sprintf "%.2f" (Price.to_float fill.price)
        ; (match fill.liquidity with Taker -> "taker" | Maker -> "maker")
        ])
  in
  String.concat ~sep:"\n" (header :: rows) ^ "\n"
;;
