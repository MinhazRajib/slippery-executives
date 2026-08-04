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

type t = Packed : (module S with type t = 'state) * 'state -> t

let advance (Packed ((module E), state)) ~bar ~resting_orders =
  let state, fills = E.on_bar_advance state ~bar ~resting_orders in
  Packed ((module E), state), fills
;;

let child_order (Packed ((module E), state)) child =
  let state, fills = E.on_child_order state child in
  Packed ((module E), state), fills
;;
