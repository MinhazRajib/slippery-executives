(** Engine B: a synthetic exchange behind the same {!Engine_intf} seam as the
    bar-based {!Fill_model}. Background agents are calibrated to each
    historical bar — three market-maker archetypes ladder both sides of a
    {!Book} around the bar's price path with depth proportional to bar volume
    and spreads that widen with bar range, while noise traders print a tape
    that tracks the bar's own volume and direction. Client orders walk the
    resting ladders through real price levels, so partial fills, depth
    exhaustion, and market impact {e emerge mechanically} — and because
    makers requote around the historical path each bar, impact is real but
    temporary (the leash).

    Everything is driven by a pure 32-bit LCG ({!Rng}): the same seed
    produces bit-identical fills natively and in the browser, which the
    server's re-run-to-verify anti-cheat depends on.

    v1 simplifications, on purpose: client limit orders never rest in the
    book (resting fills keep Engine A's strict-through-at-the-limit
    convention, so there is no queue position yet), and the background tape
    of bar [m] is printed when bar [m+1] opens — client-visible mechanics
    only ever see fresh ladders at each bar's open. *)

open! Core
open! Execlab_types
open! Execlab_market
open! Execlab_execution
open! Execlab_simulation

module Config : sig
  type t = { seed : int } [@@deriving sexp_of]

  val default : t
end

type t

val create : Config.t -> t

val on_bar_advance
  :  t
  -> bar:Market_bar.t
  -> resting_orders:Child_order.t list
  -> t * Fill.t list

val on_child_order : t -> Child_order.t -> t * Fill.t list

(** The exchange packed behind the engine seam — pass to {!Driver.run}'s
    [?engine]. *)
val engine : Config.t -> Engine_intf.t

module For_testing : sig
  (** Volume-weighted average of every print so far — background tape and
      client fills alike. The killer calibration invariant: with no client
      orders at all, this must track the historical day's own VWAP. [None]
      before anything trades. *)
  val sim_vwap : t -> float option

  (** Prints the still-pending final bar's background tape (a real run leaves
      the last bar unprinted). *)
  val finish_day : t -> t

  val book : t -> Book.t
end
