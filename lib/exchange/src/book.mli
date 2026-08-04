(** A price-aggregated limit order book of {e agent} liquidity: each side is
    a list of (price, size) levels, best first. Only the synthetic market's
    background agents quote here — a client's resting order is not displayed,
    it is tracked against the size that {e was} displayed at its price when
    it arrived (see {!size_at}) — so there is no queue to track within a
    level, only the depth a newcomer must wait behind. *)

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

(** Agent size displayed at exactly this price on [side] — what a client
    order posting there would have to queue behind. Zero if nobody is showing
    that price. *)
val size_at : t -> side:Side.t -> price:Price.t -> int

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
