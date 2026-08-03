(* One complete laboratory run: the bridge between the wizard's inputs and
   the results screen. Executes the instructions twice under identical market
   and fill-model conditions — once with the chosen algorithm, once with the
   {!Immediate} baseline — and grades every instruction with
   {!Transaction_cost}, so the results screen can show both absolute costs
   and execution value-add. *)

open! Core
open! Execlab_types
open! Execlab_market
open! Execlab_execution
open! Execlab_analytics
open! Execlab_simulation

module Algo_choice = struct
  type t = Twap [@@deriving sexp_of, compare, equal, enumerate]

  let display_name = function Twap -> "TWAP"
  let to_algorithm : t -> Algorithm_intf.t = function Twap -> (module Twap)
end

module Graded = struct
  (* One instruction's scorecard: the algorithm's grading, the baseline's,
     and the difference that is the whole point of the lab. *)
  type t =
    { instruction : Alpha_instruction.t
    ; algo : Transaction_cost.t
    ; baseline : Transaction_cost.t
    ; value_add_cents : int
    }
end

module Totals = struct
  (* Run-level sums of the per-instruction gradings, for the headline tiles.
     The accounting identity carries over: gross = net + friction +
     opportunity, summed. *)
  type t =
    { gross_cents : int
    ; net_cents : int
    ; baseline_net_cents : int
    ; value_add_cents : int
    ; requested_shares : int
    ; filled_shares : int
    }

  let of_graded (graded : Graded.t list) =
    let sum f = List.sum (module Int) graded ~f in
    { gross_cents = sum (fun g -> g.algo.gross_theoretical_pnl_cents)
    ; net_cents = sum (fun g -> g.algo.net_pnl_cents)
    ; baseline_net_cents = sum (fun g -> g.baseline.net_pnl_cents)
    ; value_add_cents = sum (fun g -> g.value_add_cents)
    ; requested_shares = sum (fun g -> Size.to_int g.algo.quantity)
    ; filled_shares = sum (fun g -> Size.to_int g.algo.filled)
    }
  ;;

  let alpha_capture t =
    if t.gross_cents > 0
    then Some (Float.of_int t.net_cents /. Float.of_int t.gross_cents)
    else None
  ;;

  let completion_rate t =
    if t.requested_shares > 0
    then Float.of_int t.filled_shares /. Float.of_int t.requested_shares
    else 0.
  ;;
end

module Output = struct
  type t =
    { day : Trading_day.t
    ; algo_name : string
    ; fill_config : Fill_model.Config.t
    ; graded : Graded.t list
    ; totals : Totals.t
    ; fills : Fill.t list (* the algorithm's, in session order *)
    ; baseline_fills : Fill.t list
    ; parents : Parent_order.t list
        (* final parent states, in instruction order — the replay screen
           reconstructs the session timeline from their children *)
    ; fills_by_parent : Fill.t list Int.Map.t
        (* the algorithm's fills grouped by instruction index *)
    ; day_vwap : float
    }
end

(* The driver reports one flat fill stream; group it back per instruction by
   walking each parent's children. Parents are indexed by instruction
   position, which is stable for the whole run. *)
let fills_by_parent (driver : Driver.t) =
  let owner_of_child =
    List.foldi
      (Order_manager.parents driver.manager)
      ~init:Order_id.Map.empty
      ~f:(fun index map parent ->
        List.fold parent.children ~init:map ~f:(fun map child ->
          Map.set map ~key:child.id ~data:index))
  in
  List.fold driver.fills ~init:Int.Map.empty ~f:(fun map fill ->
    let index = Map.find_exn owner_of_child fill.Fill.order_id in
    Map.add_multi map ~key:index ~data:fill)
  |> Map.map ~f:List.rev
;;

let grade_run
  ~day
  ~instructions
  ~(fill_config : Fill_model.Config.t)
  ~(driver : Driver.t)
  =
  let day_vwap = Day_stats.vwap day in
  let terminal_price = Benchmarks.terminal_price day in
  let by_parent = fills_by_parent driver in
  List.mapi instructions ~f:(fun index instruction ->
    let open Or_error.Let_syntax in
    let%bind arrival_price =
      Benchmarks.arrival_price
        day
        ~arrival_time:instruction.Alpha_instruction.arrival_time
    in
    Transaction_cost.create
      ~instruction
      ~fills:(Option.value (Map.find by_parent index) ~default:[])
      ~arrival_price
      ~terminal_price
      ~day_vwap
      ~half_spread:fill_config.half_spread)
  |> Or_error.combine_errors
;;

let run ~day ~instructions ~fill_config ~algo : Output.t Or_error.t =
  Or_error.try_with_join (fun () ->
    let open Or_error.Let_syntax in
    let algo_driver =
      Driver.run
        ~fill_config
        ~day
        ~instructions
        ~algorithm:(Algo_choice.to_algorithm algo)
        ()
    in
    let baseline_driver =
      Driver.run
        ~fill_config
        ~day
        ~instructions
        ~algorithm:(module Immediate)
        ()
    in
    let%bind algo_graded =
      grade_run ~day ~instructions ~fill_config ~driver:algo_driver
    in
    let%bind baseline_graded =
      grade_run ~day ~instructions ~fill_config ~driver:baseline_driver
    in
    let%bind graded =
      List.map3_exn
        instructions
        algo_graded
        baseline_graded
        ~f:(fun instruction algo_cost baseline_cost ->
          let%bind.Or_error value_add_cents =
            Transaction_cost.value_add_cents
              ~algo:algo_cost
              ~baseline:baseline_cost
          in
          Ok
            { Graded.instruction
            ; algo = algo_cost
            ; baseline = baseline_cost
            ; value_add_cents
            })
      |> Or_error.combine_errors
    in
    Ok
      { Output.day
      ; algo_name = Algo_choice.display_name algo
      ; fill_config
      ; graded
      ; totals = Totals.of_graded graded
      ; fills = algo_driver.fills
      ; baseline_fills = baseline_driver.fills
      ; parents = Order_manager.parents algo_driver.manager
      ; fills_by_parent = fills_by_parent algo_driver
      ; day_vwap = Day_stats.vwap day
      })
;;
