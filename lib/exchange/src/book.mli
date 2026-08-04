(** A price-aggregated limit order book of {e agent} liquidity: each side is
    a list of (price, size) levels, best first. Only the synthetic market's
    background agents rest here — client orders always take (v1 keeps client
    resting limits on the driver's strict-through rule, exactly as Engine A
    prices them) — so there is no queue priority to track {e within} a level,
    only between levels. *)

open! Core
open! Execlab_types

type t [@@deriving sexp_of]

val empty : t

(** Replaces one side wholesale — how makers re-quote. Duplicate prices
    aggregate; non-positive sizes drop. [side] is the side of the book: [Buy]
    = bids. *)
val set_side : t -> side:Side.t -> (Price.t * int) list -> t

(** Best price on a book side, if any liquidity rests there. *)
val best : t -> side:Side.t -> Price.t option

(** A taker walks the opposite side, best level first, until [size] is done,
    the book side runs dry, or the next level no longer crosses [limit]
    ([None] = market order). Returns the fills as (maker price, size), best
    first — depth exhaustion just yields fewer shares, never an error. *)
val take
  :  t
  -> taker_side:Side.t
  -> ?limit:Price.t
  -> size:int
  -> unit
  -> t * (Price.t * int) list
