(** The price constraint on an order: how aggressive it is allowed to be.

    This captures only the price dimension. The order's side and quantity
    live on the order itself (e.g. a future [Child_order.t]), which carries
    an [Order_type.t] as one of its fields.

    A [Limit] order at a marketable price (through the opposite side of the
    market) behaves like a market order with a safety rail — see
    {!Price.is_marketable}. *)

open! Core

type t =
  | Market (** Execute at any price; demands immediacy. *)
  | Limit of Price.t
  (** Execute only at this price or better: a buy fills at or below the
      price, a sell at or above. *)
[@@deriving sexp, bin_io, compare, equal]

val to_string : t -> string
