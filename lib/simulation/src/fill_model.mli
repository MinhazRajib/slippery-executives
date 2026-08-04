(** Engine A: the bar-based fill model. Answers "what happens to this child
    order?" using arithmetic on the current bar — no order book exists; this
    is a stateless-per-bar stand-in for a whole market, following standard
    bar-backtester conventions.

    The rules, and the approximation each one embodies:

    - Aggressive orders reference the bar's {e open} (the price at the moment
      of submission, since algorithms decide on the previous bar) and pay a
      fabricated half-spread plus square-root market impact:
      [impact_coefficient * sqrt (fill_size / bar_volume)].
    - Resting limit orders fill at their own price, as the
      {!Liquidity.Maker}, only if the bar trades {e strictly through} the
      price ([low < limit] for buys) — stricter than fill-on-touch, partially
      compensating for the unmodeled queue position.
    - Every fill is capped by the bar's participation budget
      ([max_participation * volume]), shared across all orders in the bar.

    The engine never cancels: unfilled IOC remainders are the driver's job to
    cancel (with {!Cancel_reason.Ioc_remainder}). *)

open! Core
open! Execlab_types
open! Execlab_market
open! Execlab_execution

module Config : sig
  type t =
    { half_spread : Price.t
    (** Half the fabricated bid/ask spread around the bar's open. *)
    ; max_participation : float
    (** Fraction of a bar's volume we may consume, e.g. [0.1]. *)
    ; impact_coefficient : Price.t
    (** Price penalty at 100% participation; scales with the square root of
        the participation ratio. *)
    }
  [@@deriving sexp_of]

  val default : t
end

type t [@@deriving sexp_of]

val create : Config.t -> t

(** The model packed behind the engine seam — the driver's default market. *)
val engine : Config.t -> Engine_intf.t

(** Starts a new bar: resets the participation budget, then gives each live
    resting limit order a chance to fill against the bar's range. Raises if a
    market order appears in [resting_orders] — market orders never rest. *)
val on_bar_advance
  :  t
  -> bar:Market_bar.t
  -> resting_orders:Child_order.t list
  -> t * Fill.t list

(** A just-submitted order meets the current bar: market orders and
    marketable limits fill immediately (as the taker, never beyond their
    limit); non-marketable limits return no fills and should be left resting
    by the driver. Raises before the first bar. *)
val on_child_order : t -> Child_order.t -> t * Fill.t list

(** The fabricated bid/ask around the current bar's open, with the remaining
    participation budget as displayed depth. [None] before the first bar. *)
val synthetic_bbo : t -> Bbo.t option
