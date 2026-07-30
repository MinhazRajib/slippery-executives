(** Benchmark prices for grading an execution: the reference points the
    transaction-cost metrics compare fills against.

    Three benchmarks anchor the metric tree (see the analytics section of
    [context.md]):

    - the {e arrival price} — the market price at the instant an instruction
      activated; {!Transaction_cost} measures implementation shortfall
      against it;
    - the {e terminal price} — where the session ended; it marks unfilled
      shares and defines theoretical alpha P&L;
    - the day VWAP — how the crowd traded; provided by {!Day_stats.vwap} and
      deliberately not duplicated here.

    This module owns the bar-data conventions (which minute, which side of
    the bar), so {!Transaction_cost} stays pure arithmetic over prices it is
    handed. *)

open! Core
open! Execlab_types
open! Execlab_market

(** Open of the first session minute at or after [arrival_time]: the
    decision-moment benchmark. Errors if the arrival time falls after the
    final session minute. *)
val arrival_price
  :  Trading_day.t
  -> arrival_time:Time_ns.Ofday.t
  -> Price.t Or_error.t

(** Close of the session's final minute (15:59): the mark for shares still
    open at the end of a run and the price at which theoretical alpha P&L is
    realized. *)
val terminal_price : Trading_day.t -> Price.t
