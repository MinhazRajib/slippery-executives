(** An execution event on one of our child orders, produced by whichever fill
    engine is running (bar-based fill model or synthetic exchange).

    Fills are small immutable facts: one child order got [size] shares at
    [price] at [time]. A single child order may produce several fills. The
    portfolio and analytics layers are driven entirely by the stream of
    fills. *)

open! Core

type t =
  { fill_id : int
  (** Unique within a run, assigned sequentially by the fill engine. *)
  ; symbol : Symbol.t
  ; price : Price.t (** The price at which the trade occurred. *)
  ; size : Size.t (** The number of shares/units traded. *)
  ; order_id : Order_id.t
  ; side : Side.t
  ; time : Time_ns.Ofday.t
  ; liquidity : Liquidity.t
  }
[@@deriving sexp, bin_io]

(** Renders a fill as an event-log line, e.g.
    ["10:15:08 BUY 700 NVDA @ $150.26 (Taker, fill 3, order 42)"]. *)
val to_string : t -> string

(** {2 Convenience accessors} *)

(** The total notional value of the fill in cents (price * size). *)
val notional_cents : t -> int
