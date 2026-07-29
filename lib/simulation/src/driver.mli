(** The simulation driver: conducts one session, bar by bar.

    Per bar, in order: activate parents whose arrival time has come (sampling
    the arrival price from the bar's open), expire parents whose deadline has
    passed, let each active parent's algorithm decide from the {e previous}
    bar, then let the bar trade — resting limit orders first (they were there
    first), then the algorithm's new submissions, with IOC/market remainders
    canceled immediately. Fills flow back into the order manager as they
    happen.

    This is the batch runner; a paced step-on-demand playback wrapper for the
    UI comes later and will reuse the same per-bar step. *)

open! Core
open! Execlab_types
open! Execlab_market
open! Execlab_execution

type t =
  { manager : Order_manager.t (** Final state of every parent and child. *)
  ; fills : Fill.t list (** Every fill of the session, in order. *)
  }
[@@deriving sexp_of]

val run
  :  ?fill_config:Fill_model.Config.t
       (** default {!Fill_model.Config.default} *)
  -> day:Trading_day.t
  -> instructions:Alpha_instruction.t list
  -> algorithm:Algorithm_intf.t
  -> unit
  -> t
