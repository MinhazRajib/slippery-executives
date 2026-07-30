open! Core
open! Execlab_types

type state = { submitted : bool }

let name = "immediate"
let init ~parent:(_ : Parent_order.t) = { submitted = false }

let on_bar state (context : Algorithm_intf.Context.t) =
  let parent = context.parent in
  let remaining = Parent_order.remaining parent in
  if state.submitted || Size.( <= ) remaining Size.zero
  then state, []
  else (
    let request =
      Or_error.ok_exn
        (Child_order.Request.create
           ~symbol:parent.instruction.Alpha_instruction.symbol
           ~side:parent.instruction.Alpha_instruction.side
           ~quantity:remaining
           ~order_type:Order_type.Market
           ~time_in_force:Time_in_force.IOC)
    in
    { submitted = true }, [ Algorithm_intf.Action.Submit request ])
;;
