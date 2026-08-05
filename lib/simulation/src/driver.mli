(** The simulation driver: conducts one trading date, minute by minute,
    across every symbol the alpha touches.

    Per minute, in order: activate parents whose arrival time has come
    (sampling the arrival price from their own symbol's bar), expire parents
    whose deadline has passed, let each active parent's algorithm decide from
    the {e previous} bar of {e its own} symbol, then let the minute trade —
    resting limit orders first (they were there first), then the algorithm's
    new submissions, with IOC/market remainders canceled immediately. Fills
    flow back into the order manager as they happen.

    Every symbol has its own engine, so a minute's participation budget — or
    a whole order book, under the synthetic exchange — belongs to one name
    and cannot be spent by trading in another. Symbols share only the clock
    and the algorithm, which is what makes a multi-symbol run one run rather
    than several stitched together.

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
  -> ?engine_for:(Symbol.t -> Engine_intf.t)
       (** the market each symbol trades in; default {!Fill_model.engine} of
           [fill_config], which [engine_for] overrides entirely. Called once
           per symbol, so engines never share state across names *)
  -> universe:Universe.t
  -> instructions:Alpha_instruction.t list
  -> algorithm:Algorithm_intf.t
  -> unit
  -> t

(*_ Raises if an instruction names a symbol the universe has no session for;
    validate with {!Universe.mem} first. *)
