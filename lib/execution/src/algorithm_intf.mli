(** The common interface every execution algorithm implements — the socket
    that makes TWAP, VWAP, POV, and implementation shortfall swappable and
    therefore comparable under identical conditions.

    On every simulation tick the driver hands the algorithm a {!Context}
    describing what it may know, and the algorithm answers with {!Action}s.
    Algorithms are stateful: [on_bar] threads a [state] value (e.g. TWAP's
    slice schedule) through the run. *)

open! Core
open! Execlab_types
open! Execlab_market

module Action : sig
  type t =
    | Submit of Child_order.Request.t
    | Cancel of Order_id.t
  [@@deriving sexp_of, compare, equal]
end

module Context : sig
  type t =
    { now : Time_ns.Ofday.t
    ; previous_bar : Market_bar.t
    (** The last {e completed} bar — never the bar currently forming. Fills
        from a submitted order happen in the current bar, which the algorithm
        cannot see: no look-ahead. *)
    ; parent : Parent_order.t
    ; live_orders : Child_order.t list
    }
  [@@deriving sexp_of]
end

module type S = sig
  type state

  val name : string
  val init : parent:Parent_order.t -> state
  val on_bar : state -> Context.t -> state * Action.t list
end

(** A packed algorithm, ready for the driver. Concrete algorithms expose
    [create : Config.t -> t], capturing their config inside the module. *)
type t = (module S)
