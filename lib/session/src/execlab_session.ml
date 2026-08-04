open! Core
open! Execlab_types
open! Execlab_market
open! Execlab_execution
open! Execlab_simulation
open! Execlab_analytics

module Engine_choice = struct
  type t =
    | Bar_model
    | Synthetic of { seed : int }
  [@@deriving sexp, equal]
end

module Params = struct
  type t =
    { fill_config : Fill_model.Config.t
    ; pov_rate : float
    ; is_urgency : float
    ; engine : Engine_choice.t
    }

  let default =
    { fill_config = Fill_model.Config.default
    ; pov_rate = 0.0015
    ; is_urgency = 2.0
    ; engine = Engine_choice.Bar_model
    }
  ;;
end

let forecast_profile ~(day : Trading_day.t) ~forecast_days =
  match Day_stats.average_volume_profile forecast_days with
  | Ok profile -> profile
  | Error (_ : Error.t) -> Day_stats.volume_profile day
;;

let algorithm_named
  ~(day : Trading_day.t)
  ~forecast_days
  ~(params : Params.t)
  = function
  | "immediate" -> Ok (module Immediate : Algorithm_intf.S)
  | "twap" -> Ok (module Twap : Algorithm_intf.S)
  | "vwap" ->
    let profile =
      List.map2_exn
        day.bars
        (forecast_profile ~day ~forecast_days)
        ~f:(fun bar weight -> bar.Market_bar.time, weight)
    in
    Ok (Vwap.create ~profile)
  | "pov" -> Ok (Pov.create ~participation_rate:params.pov_rate ())
  | "is" ->
    Ok (Implementation_shortfall.create ~urgency:params.is_urgency ())
  | other ->
    Or_error.error_s
      [%message
        "unknown algorithm"
          (other : string)
          ~known:"twap, vwap, pov, is, immediate"]
;;

module Graded = struct
  type t =
    { grading : Transaction_cost.t
    ; baseline : Transaction_cost.t
    ; value_add_cents : int
    }
end

module Outcome = struct
  type t =
    { algo_result : Driver.t
    ; baseline_result : Driver.t
    ; graded : Graded.t list
    }

  let total t f = List.sum (module Int) t.graded ~f

  let value_add_cents t =
    total t (fun graded -> graded.Graded.value_add_cents)
  ;;

  let net_cents t =
    total t (fun graded ->
      graded.Graded.grading.Transaction_cost.net_pnl_cents)
  ;;

  let gross_cents t =
    total t (fun graded ->
      graded.Graded.grading.Transaction_cost.gross_theoretical_pnl_cents)
  ;;

  let shortfall_cents t =
    total t (fun graded ->
      graded.Graded.grading.Transaction_cost.friction_cost_cents)
  ;;

  let alpha_capture t =
    let gross = gross_cents t in
    if gross > 0 then Some (net_cents t // gross) else None
  ;;
end

let grade ~day ~attribution_half_spread (result : Driver.t) =
  let day_vwap = Day_stats.vwap day in
  let terminal_price = Benchmarks.terminal_price day in
  let half_spread = attribution_half_spread in
  List.map (Order_manager.parents result.manager) ~f:(fun parent ->
    let open Or_error.Let_syntax in
    let instruction = parent.Parent_order.instruction in
    let fills =
      let ids =
        Order_id.Set.of_list
          (List.map parent.children ~f:(fun child -> child.Child_order.id))
      in
      List.filter result.fills ~f:(fun fill ->
        Set.mem ids fill.Fill.order_id)
    in
    let%bind arrival_price =
      Benchmarks.arrival_price
        day
        ~arrival_time:instruction.Alpha_instruction.arrival_time
    in
    Transaction_cost.create
      ~instruction
      ~fills
      ~day
      ~arrival_price
      ~terminal_price
      ~day_vwap
      ~half_spread)
  |> Or_error.combine_errors
;;

let run ~day ~forecast_days ~instructions ~algo_name ~(params : Params.t) =
  let open Or_error.Let_syntax in
  let%bind algorithm =
    algorithm_named ~day ~forecast_days ~params algo_name
  in
  let engine () =
    match params.engine with
    | Engine_choice.Bar_model -> Fill_model.engine params.fill_config
    | Synthetic { seed } -> Execlab_exchange.Synthetic_market.engine { seed }
  in
  let run_one algorithm =
    Driver.run ~day ~instructions ~algorithm ~engine:(engine ()) ()
  in
  let algo_result = run_one algorithm in
  let baseline_result = run_one (module Immediate) in
  (* Spread attribution follows the engine that produced the fills. Engine A
     prices takers at open +/- the configured half-spread, so that knob is
     the exact toll; Engine B's costs are the ladder walked level by level —
     there is no separate spread toll, so the whole beyond-drift residual is
     attributed to impact. *)
  let attribution_half_spread =
    match params.engine with
    | Engine_choice.Bar_model ->
      params.fill_config.Fill_model.Config.half_spread
    | Synthetic { seed = (_ : int) } -> Price.of_int_cents 0
  in
  let%bind algo_gradings = grade ~day ~attribution_half_spread algo_result in
  let%bind baseline_gradings =
    grade ~day ~attribution_half_spread baseline_result
  in
  let%map graded =
    List.map2_exn algo_gradings baseline_gradings ~f:(fun algo baseline ->
      let%map.Or_error value_add_cents =
        Transaction_cost.value_add_cents ~algo ~baseline
      in
      { Graded.grading = algo; baseline; value_add_cents })
    |> Or_error.combine_errors
  in
  { Outcome.algo_result; baseline_result; graded }
;;
