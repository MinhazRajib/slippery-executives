(** The desk's job ticket for one alpha instruction: its lifecycle and
    running totals, and the unit every execution analytic is computed over.

    State machine:
    {v Pending -> Active -> (Completed | Expired) v}

    A parent activates when the simulation clock reaches the instruction's
    arrival time, sampling {!field:arrival_price} — the benchmark that
    implementation shortfall is measured against. The order manager is the
    only component that calls the [_exn] transition functions; each raises if
    called in the wrong state, and {!add_child_exn} enforces the overfill
    invariant [filled + working <= total]. *)

open! Core
open! Execlab_types

module Status : sig
  type t =
    | Pending
    | Active
    | Completed
    | Expired
  [@@deriving sexp_of, compare, equal]
end

type t = private
  { instruction : Alpha_instruction.t
  ; status : Status.t
  ; arrival_price : Price.t option (** [None] until activation. *)
  ; filled : Size.t
  ; children : Child_order.t list (** Newest first; live and dead. *)
  }
[@@deriving sexp_of, compare, equal]

val create : Alpha_instruction.t -> t

(** Samples the arrival-price benchmark and starts work. Raises unless
    [Pending]. *)
val activate_exn : t -> arrival_price:Price.t -> t

(** Raises unless [Active], if the child's symbol does not match the
    instruction, or if the child would breach [filled + working <= total]. *)
val add_child_exn : t -> Child_order.t -> t

(** Routes the fill to the matching child and adds it to [filled];
    auto-transitions to [Completed] when [filled] reaches the instruction's
    quantity. Raises unless [Active] or if no child matches the fill's order
    id. *)
val apply_fill_exn : t -> Fill.t -> t

(** Deadline passed: cancels every live child with [Deadline_expired] — an
    expired parent leaves no live orders in the market. Raises unless
    [Active]. *)
val expire_exn : t -> t

(** {2 Accessors} *)

val total : t -> Size.t

(** Sum of live children's remaining quantity. *)
val working : t -> Size.t

(** [total - filled - working]: quantity not yet filled or in flight. *)
val remaining : t -> Size.t

val live_children : t -> Child_order.t list
val is_active : t -> bool
