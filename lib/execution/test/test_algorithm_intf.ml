open! Core
open! Execlab_types
open! Execlab_market
open Execlab_execution

(* Exercises the interface through the real [Immediate] baseline: state
   threads through [on_bar], and packing/unpacking the first-class module
   works. *)

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
