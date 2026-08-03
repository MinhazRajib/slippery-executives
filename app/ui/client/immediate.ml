(* The naive baseline every run is graded against: from the moment a parent
   activates, demand its whole remaining quantity with market IOC orders,
   re-submitting each bar until the participation cap lets it all through.
   "Execution value-add" on the results screen is the algorithm's net P&L
   minus this strategy's, under identical market and fill-model conditions. *)

open! Core
open! Execlab_types
open! Execlab_execution

type state = unit

let name = "immediate"
let init ~parent:(_ : Parent_order.t) = ()

let on_bar () (context : Algorithm_intf.Context.t) =
  let parent = context.parent in
  let remaining = Size.to_int (Parent_order.remaining parent) in
  if remaining <= 0
  then (), []
  else (
    let instruction = parent.instruction in
    let request =
      Or_error.ok_exn
        (Child_order.Request.create
           ~symbol:instruction.Alpha_instruction.symbol
           ~side:instruction.Alpha_instruction.side
           ~quantity:(Size.of_int remaining)
           ~order_type:Order_type.Market
           ~time_in_force:Time_in_force.IOC)
    in
    (), [ Algorithm_intf.Action.Submit request ])
;;
