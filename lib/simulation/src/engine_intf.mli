(** The fill-engine interface: what the {!Driver} needs from a market, and
    nothing else — so Engine A (the {!Fill_model} bar calculator) and Engine
    B (the synthetic exchange in [lib/exchange]) are interchangeable and
    algorithms can never tell which world they are trading in.

    The contract, mirrored from {!Fill_model}: [on_bar_advance] is called
    once per bar {e before} that bar's submissions, granting fills to the
    driver's resting limit orders; [on_child_order] prices one new marketable
    order. Engines never cancel — IOC remainders are the driver's job. *)

open! Core
open! Execlab_types
open! Execlab_market
open! Execlab_execution

module type S = sig
  type t

  val on_bar_advance
    :  t
    -> bar:Market_bar.t
    -> resting_orders:Child_order.t list
    -> t * Fill.t list

  val on_child_order : t -> Child_order.t -> t * Fill.t list
end

(** An engine packed with its state, {!Algorithm_intf.t}-style. *)
type t = Packed : (module S with type t = 'state) * 'state -> t

val advance
  :  t
  -> bar:Market_bar.t
  -> resting_orders:Child_order.t list
  -> t * Fill.t list

val child_order : t -> Child_order.t -> t * Fill.t list
