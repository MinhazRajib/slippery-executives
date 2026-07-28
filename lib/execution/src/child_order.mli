(** A single order sent to the market: the atomic unit of market interaction,
    and one row of the child-order blotter.

    An algorithm expresses intent as a {!Request}; the order manager turns an
    accepted request into a live [t] (assigning the {!Order_id.t}) and is the
    only component that advances its state via {!apply_fill_exn} and
    {!cancel_exn}. Every {!Fill.t} points back at one of these by id. *)

open! Core
open! Execlab_types

module Status : sig
  type t =
    | Live
    | Filled
    | Canceled of Cancel_reason.t
  [@@deriving sexp_of, compare, equal]
end

module Request : sig
  (** What the algorithm asked for — immutable; the blotter's "intent"
      column. *)
  type t =
    { symbol : Symbol.t
    ; side : Side.t
    ; quantity : Size.t
    ; order_type : Order_type.t
    ; time_in_force : Time_in_force.t (** Ignored for [Market] orders. *)
    }
  [@@deriving sexp_of, compare, equal]

  (** Errors if [quantity] is not positive. *)
  val create
    :  symbol:Symbol.t
    -> side:Side.t
    -> quantity:Size.t
    -> order_type:Order_type.t
    -> time_in_force:Time_in_force.t
    -> t Or_error.t
end

type t = private
  { id : Order_id.t
  ; request : Request.t
  ; submitted_at : Time_ns.Ofday.t
  ; remaining : Size.t
  ; status : Status.t
  }
[@@deriving sexp_of, compare, equal]

(** A fresh live order: [remaining] starts at the request's quantity. Only
    the order manager calls this — it owns the id generator. *)
val create
  :  request:Request.t
  -> id:Order_id.t
  -> submitted_at:Time_ns.Ofday.t
  -> t

(** Decrements [remaining]; the order becomes [Filled] when it reaches zero.
    Raises if the order is not live, or if [quantity] is zero, negative, or
    exceeds [remaining] — those are fill-engine bugs. *)
val apply_fill_exn : t -> quantity:Size.t -> t

(** Marks the order [Canceled], keeping [remaining] as the unfilled size.
    Raises if the order is not live (double-cancel is a manager bug). *)
val cancel_exn : t -> reason:Cancel_reason.t -> t

val is_live : t -> bool
