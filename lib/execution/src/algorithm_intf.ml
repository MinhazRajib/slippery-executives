open! Core
open! Execlab_types
open! Execlab_market

module Action = struct
  type t =
    | Submit of Child_order.Request.t
    | Cancel of Order_id.t
  [@@deriving sexp_of, compare, equal]
end

module Context = struct
  type t =
    { now : Time_ns.Ofday.t
    ; previous_bar : Market_bar.t
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

type t = (module S)
