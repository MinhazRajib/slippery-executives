open! Core
open! Execlab_types

type state = unit

let name = "twap"
let init ~parent:(_ : Parent_order.t) = ()

(* Schedule target at minute [m] of an [n]-minute window is total * m / n in
   integer math, so the target reaches exactly [total] at the deadline. *)
let on_bar () (context : Algorithm_intf.Context.t) =
  let parent = context.parent in
  let instruction = parent.instruction in
  let arrival = instruction.Alpha_instruction.arrival_time in
  let deadline = instruction.Alpha_instruction.deadline in
  let minutes between = Float.to_int (Time_ns.Span.to_min between) in
  let total_minutes = minutes (Time_ns.Ofday.diff deadline arrival) in
  let elapsed_minutes = minutes (Time_ns.Ofday.diff context.now arrival) in
  let total = Size.to_int (Parent_order.total parent) in
  let target =
    if total_minutes = 0
    then total
    else total * elapsed_minutes / total_minutes
  in
  let spoken_for =
    Size.to_int parent.filled + Size.to_int (Parent_order.working parent)
  in
  let deficit =
    Int.min
      (target - spoken_for)
      (Size.to_int (Parent_order.remaining parent))
  in
  if deficit <= 0
  then (), []
  else (
    let request =
      Or_error.ok_exn
        (Child_order.Request.create
           ~symbol:instruction.Alpha_instruction.symbol
           ~side:instruction.Alpha_instruction.side
           ~quantity:(Size.of_int deficit)
           ~order_type:Order_type.Market
           ~time_in_force:Time_in_force.IOC)
    in
    (), [ Algorithm_intf.Action.Submit request ])
;;
