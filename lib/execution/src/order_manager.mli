(** The middle office: owns the {!Order_id.Generator} and every parent order,
    and is the single gateway through which algorithm actions and fills
    change execution state.

    The driver calls {!activate_due} and {!expire_due} as the clock advances,
    routes each algorithm [Submit]/[Cancel] through
    {!submit_exn}/{!cancel_exn}, and feeds every {!Fill.t} to
    {!apply_fill_exn}. Parents are indexed by their position in the
    instruction list given to {!create}; the set never changes during a run,
    so indices are stable. *)

open! Core
open! Execlab_types

type t [@@deriving sexp_of]

val create : instructions:Alpha_instruction.t list -> t
val parents : t -> Parent_order.t list
val parent_exn : t -> int -> Parent_order.t

(** Activates every pending parent whose arrival time is at or before [now],
    sampling its arrival price via [price_for]. *)
val activate_due
  :  t
  -> now:Time_ns.Ofday.t
  -> price_for:(Symbol.t -> Price.t)
  -> t

(** Assigns the next order id, creates the live child, and adds it to the
    parent — raising if the parent isn't active or the child would breach the
    overfill invariant. Returns the child so the driver can hand it to the
    fill engine. *)
val submit_exn
  :  t
  -> parent_index:int
  -> request:Child_order.Request.t
  -> now:Time_ns.Ofday.t
  -> t * Child_order.t

(** Cancels the live child with this id, wherever it is. Raises if no parent
    has such a child or it is not live. *)
val cancel_exn : t -> order_id:Order_id.t -> reason:Cancel_reason.t -> t

(** Routes a fill to the parent owning [fill.order_id]. Raises if none does. *)
val apply_fill_exn : t -> Fill.t -> t

(** Expires every active parent whose deadline is strictly before [now],
    cancelling its live children with [Deadline_expired]. The deadline minute
    itself may still trade. *)
val expire_due : t -> now:Time_ns.Ofday.t -> t
