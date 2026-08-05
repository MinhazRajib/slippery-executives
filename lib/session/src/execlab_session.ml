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
    ; patience : float
    ; engine : Engine_choice.t
    }

  let default =
    { fill_config = Fill_model.Config.default
    ; pov_rate = 0.0015
    ; is_urgency = 2.0
    ; patience = 0.5
    ; engine = Engine_choice.Bar_model
    }
  ;;
end

(* Everything about the market a run was graded in, as a short digest.
   Recalibrating any of it changes the digest, which keeps runs graded under
   different physics off the same leaderboard instead of silently ranking
   them against each other. The synthetic seed is excluded: it is derived per
   board, not part of the market's calibration. *)
let physics_fingerprint (params : Params.t) =
  let engine =
    match params.engine with
    | Engine_choice.Bar_model -> Sexp.Atom "bar"
    | Synthetic { seed = (_ : int) } ->
      [%sexp
        Synthetic
          ({ Execlab_exchange.Synthetic_market.Config.default with seed = 0 }
           : Execlab_exchange.Synthetic_market.Config.t)]
  in
  let sexp =
    [%message
      ""
        ~fill:(params.fill_config : Fill_model.Config.t)
        ~engine:(engine : Sexp.t)]
  in
  String.prefix (Md5.to_hex (Md5.digest_string (Sexp.to_string sexp))) 8
;;

let forecast_profile ~(day : Trading_day.t) ~forecast_days =
  match Day_stats.average_volume_profile forecast_days with
  | Ok profile -> profile
  | Error (_ : Error.t) -> Day_stats.volume_profile day
;;

(* One forecast per symbol the run touches: a volume curve belongs to a
   stock, not to a run. *)
let forecast_profiles ~universe ~forecast_days =
  Symbol.Map.of_alist_exn
    (List.map (Universe.days universe) ~f:(fun (day : Trading_day.t) ->
       let others =
         Option.value (Map.find forecast_days day.symbol) ~default:[]
       in
       ( day.symbol
       , List.map2_exn
           day.bars
           (forecast_profile ~day ~forecast_days:others)
           ~f:(fun bar weight -> bar.Market_bar.time, weight) )))
;;

let algorithm_named ~universe ~forecast_days ~(params : Params.t) = function
  | "immediate" -> Ok (module Immediate : Algorithm_intf.S)
  | "twap" -> Ok (module Twap : Algorithm_intf.S)
  | "vwap" ->
    Ok (Vwap.create ~profiles:(forecast_profiles ~universe ~forecast_days))
  | "pov" -> Ok (Pov.create ~participation_rate:params.pov_rate ())
  | "is" ->
    Ok (Implementation_shortfall.create ~urgency:params.is_urgency ())
  | "adaptive" -> Ok (Adaptive.create ~patience:params.patience ())
  | other ->
    Or_error.error_s
      [%message
        "unknown algorithm"
          (other : string)
          ~known:"twap, vwap, pov, is, adaptive, immediate"]
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

(* A parent is graded against its own symbol's session: its own arrival
   price, its own closing price, its own day VWAP. Nothing about a
   multi-symbol run pools those, because a benchmark is a property of the
   market the order traded in. *)
let grade ~universe ~attribution_half_spread (result : Driver.t) =
  let half_spread = attribution_half_spread in
  List.map (Order_manager.parents result.manager) ~f:(fun parent ->
    let open Or_error.Let_syntax in
    let instruction = parent.Parent_order.instruction in
    let day =
      Universe.day_exn universe instruction.Alpha_instruction.symbol
    in
    let day_vwap = Day_stats.vwap day in
    let terminal_price = Benchmarks.terminal_price day in
    let fills =
      let ids =
        Order_id.Set.of_list
          (List.map parent.children ~f:(fun child -> child.Child_order.id))
      in
      List.filter result.fills ~f:(fun fill ->
        Set.mem ids fill.Fill.order_id)
    in
    (* The benchmark is the price the parent actually activated at, not a
       second lookup that could disagree with it; {!Benchmarks} answers only
       for a parent that never activated at all. *)
    let%bind arrival_price =
      match parent.Parent_order.arrival_price with
      | Some price -> Ok price
      | None ->
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

let run
  ~universe
  ~forecast_days
  ~instructions
  ~algo_name
  ~(params : Params.t)
  =
  let open Or_error.Let_syntax in
  let%bind () =
    match
      List.find instructions ~f:(fun instruction ->
        not (Universe.mem universe instruction.Alpha_instruction.symbol))
    with
    | None -> Ok ()
    | Some instruction ->
      Or_error.error_s
        [%message
          "the alpha names a symbol this run has no session for"
            ~symbol:(instruction.Alpha_instruction.symbol : Symbol.t)
            ~loaded:(Universe.symbols universe : Symbol.t list)]
  in
  let%bind algorithm =
    algorithm_named ~universe ~forecast_days ~params algo_name
  in
  (* One engine per symbol. A synthetic run derives each symbol's seed from
     the run's own, so two names never share a random stream — a coincidence
     of ladders and noise across unrelated stocks would be the one thing a
     synthetic market must not invent — while the run as a whole stays
     reproducible from a single number. *)
  let engine_for symbol =
    match params.engine with
    | Engine_choice.Bar_model -> Fill_model.engine params.fill_config
    | Synthetic { seed } ->
      let seed =
        String.fold (Symbol.to_string symbol) ~init:seed ~f:(fun acc c ->
          ((acc * 31) + Char.to_int c) % 100_003)
      in
      Execlab_exchange.Synthetic_market.engine
        { Execlab_exchange.Synthetic_market.Config.default with seed }
  in
  let run_one algorithm =
    Driver.run ~universe ~instructions ~algorithm ~engine_for ()
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
  let%bind algo_gradings =
    grade ~universe ~attribution_half_spread algo_result
  in
  let%bind baseline_gradings =
    grade ~universe ~attribution_half_spread baseline_result
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
