open! Core
open! Execlab_types
open! Execlab_market
open Execlab_execution

(* The simplest possible algorithm: submit the parent's full remaining
   quantity as a market order on the first bar, then do nothing. This is a
   preview of the immediate-execution baseline, and it proves the interface
   is usable: state threads through [on_bar], config-free. *)
module Immediate = struct
  type state = { submitted : bool }

  let name = "immediate"
  let init ~parent:(_ : Parent_order.t) = { submitted = false }

  let on_bar state (context : Algorithm_intf.Context.t) =
    if state.submitted
    then state, []
    else (
      let parent = context.parent in
      let request =
        Or_error.ok_exn
          (Child_order.Request.create
             ~symbol:parent.instruction.Alpha_instruction.symbol
             ~side:parent.instruction.Alpha_instruction.side
             ~quantity:(Parent_order.remaining parent)
             ~order_type:Order_type.Market
             ~time_in_force:Time_in_force.IOC)
      in
      { submitted = true }, [ Algorithm_intf.Action.Submit request ])
  ;;
end

let parent =
  let instruction =
    Or_error.ok_exn
      (Alpha_instruction.create
         ~arrival_time:(Time_ns.Ofday.of_string "10:05:00")
         ~symbol:(Symbol.of_string "NVDA")
         ~side:Side.Buy
         ~quantity:(Size.of_int 1000)
         ~deadline:(Time_ns.Ofday.of_string "11:00:00"))
  in
  Parent_order.activate_exn
    (Parent_order.create instruction)
    ~arrival_price:(Price.of_int_cents 15000)
;;

let context =
  let previous_bar =
    Or_error.ok_exn
      (Market_bar.create
         ~time:(Time_ns.Ofday.of_string "10:04:00")
         ~open_:(Price.of_int_cents 15000)
         ~high:(Price.of_int_cents 15010)
         ~low:(Price.of_int_cents 14990)
         ~close:(Price.of_int_cents 15005)
         ~volume:(Size.of_int 10000))
  in
  { Algorithm_intf.Context.now = Time_ns.Ofday.of_string "10:05:00"
  ; previous_bar
  ; parent
  ; live_orders = []
  }
;;

let%expect_test "an algorithm threads state and emits actions" =
  let algo : Algorithm_intf.t = (module Immediate) in
  let module A = (val algo) in
  printf "%s\n" A.name;
  let state = A.init ~parent in
  let state, actions = A.on_bar state context in
  print_s [%sexp (actions : Algorithm_intf.Action.t list)];
  let (_ : A.state), actions = A.on_bar state context in
  print_s [%sexp (actions : Algorithm_intf.Action.t list)];
  [%expect
    {|
    immediate
    ((Submit
      ((symbol NVDA) (side Buy) (quantity 1000) (order_type Market)
       (time_in_force IOC))))
    ()
    |}]
;;
