(** One run of the laboratory, from validated inputs to graded outcome — the
    shared pipeline behind the CLI, the browser client, and the server, so
    the three fronts cannot drift apart. Pure OCaml, no filesystem: callers
    load the day and the forecast material themselves
    ({!Execlab_market.Data_loader} natively, the embedded catalog in the
    browser). *)

open! Core
open! Execlab_types
open! Execlab_market
open! Execlab_execution
open! Execlab_simulation
open! Execlab_analytics

module Engine_choice : sig
  (** Which market to trade in: Engine A's bar calculator, or Engine B's
      synthetic exchange (seeded, so a run is reproducible — the same config
      yields bit-identical fills everywhere). *)
  type t =
    | Bar_model
    | Synthetic of { seed : int }
  [@@deriving sexp, equal]
end

module Params : sig
  (** The fill model's knobs plus each algorithm's own. [fill_config] prices
      Engine A and is its grading's spread attribution; a synthetic run's
      grading attributes zero spread — the exchange has no configured toll,
      so everything beyond drift is impact. *)
  type t =
    { fill_config : Fill_model.Config.t
    ; pov_rate : float (** POV's share of observed tape volume *)
    ; is_urgency : float (** IS front-loading; [0.] is the TWAP limit *)
    ; engine : Engine_choice.t
    }

  val default : t
end

(** The average profile of [forecast_days] (the honest leave-one-out forecast
    for VWAP), falling back to [day]'s own curve when there is nothing to
    average. *)
val forecast_profile
  :  day:Trading_day.t
  -> forecast_days:Trading_day.t list
  -> float list

(** Errors on an unknown name; ["vwap"] builds its schedule from
    {!forecast_profile}. *)
val algorithm_named
  :  day:Trading_day.t
  -> forecast_days:Trading_day.t list
  -> params:Params.t
  -> string
  -> Algorithm_intf.t Or_error.t

module Graded : sig
  (** One instruction's grade, the immediate baseline's grade of the same
      instruction, and the value-add between them. *)
  type t =
    { grading : Transaction_cost.t
    ; baseline : Transaction_cost.t
    ; value_add_cents : int
    }
end

module Outcome : sig
  type t =
    { algo_result : Driver.t
    ; baseline_result : Driver.t
    ; graded : Graded.t list (** in instruction order *)
    }

  val value_add_cents : t -> int
  val net_cents : t -> int
  val gross_cents : t -> int
  val shortfall_cents : t -> int

  (** [net / gross] across the whole run; [None] unless gross is positive. *)
  val alpha_capture : t -> float option
end

(** Runs [algo_name] and the immediate baseline over the same instructions
    and grades both. Every instruction must name [day]'s own symbol — the
    check lives here so all three fronts (CLI, browser, server) enforce it
    identically. Errors bubble up from the algorithm name, the benchmarks,
    and the grading — never raises on user input. *)
val run
  :  day:Trading_day.t
  -> forecast_days:Trading_day.t list
  -> instructions:Alpha_instruction.t list
  -> algo_name:string
  -> params:Params.t
  -> Outcome.t Or_error.t
