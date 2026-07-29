open! Core
open! Execlab_types
open! Execlab_market
open Execlab_execution

(* Fixture: buy 1,000 NVDA between 10:05 and 11:00 -- 55 minutes, so the
   schedule target at minute m is 1000 * m / 55 (integer math). *)

let instruction ~deadline =
  Or_error.ok_exn
    (Alpha_instruction.create
       ~arrival_time:(Time_ns.Ofday.of_string "10:05:00")
       ~symbol:(Symbol.of_string "NVDA")
       ~side:Side.Buy
       ~quantity:(Size.of_int 1000)
       ~deadline:(Time_ns.Ofday.of_string deadline))
;;

let active ~deadline =
  Parent_order.activate_exn
    (Parent_order.create (instruction ~deadline))
    ~arrival_price:(Price.of_int_cents 15000)
;;

let child ~id ~quantity =
  let request =
    Or_error.ok_exn
      (Child_order.Request.create
         ~symbol:(Symbol.of_string "NVDA")
         ~side:Side.Buy
         ~quantity:(Size.of_int quantity)
         ~order_type:Order_type.Market
         ~time_in_force:Time_in_force.IOC)
  in
  Child_order.create
    ~request
    ~id:(Order_id.For_testing.of_int id)
    ~submitted_at:(Time_ns.Ofday.of_string "10:06:00")
;;

let fill ~order_id ~size =
  { Fill.fill_id = 1
  ; symbol = Symbol.of_string "NVDA"
  ; price = Price.of_int_cents 15000
  ; size = Size.of_int size
  ; order_id = Order_id.For_testing.of_int order_id
  ; side = Side.Buy
  ; time = Time_ns.Ofday.of_string "10:06:30"
  ; liquidity = Liquidity.Taker
  }
;;

let previous_bar =
  Or_error.ok_exn
    (Market_bar.create
       ~time:(Time_ns.Ofday.of_string "10:04:00")
       ~open_:(Price.of_int_cents 15000)
       ~high:(Price.of_int_cents 15000)
       ~low:(Price.of_int_cents 15000)
       ~close:(Price.of_int_cents 15000)
       ~volume:(Size.of_int 10000))
;;

let actions ~now parent =
  let context =
    { Algorithm_intf.Context.now = Time_ns.Ofday.of_string now
    ; previous_bar
    ; parent
    ; live_orders = Parent_order.live_children parent
    }
  in
  let (_ : Twap.state), actions = Twap.on_bar (Twap.init ~parent) context in
  print_s [%sexp (actions : Algorithm_intf.Action.t list)]
;;

let%expect_test "at arrival the target is zero: no action" =
  actions ~now:"10:05:00" (active ~deadline:"11:00:00");
  [%expect {| () |}]
;;

let%expect_test "one minute in: submit the first slice (1000*1/55 = 18)" =
  actions ~now:"10:06:00" (active ~deadline:"11:00:00");
  [%expect
    {|
    ((Submit
      ((symbol NVDA) (side Buy) (quantity 18) (order_type Market)
       (time_in_force IOC))))
    |}]
;;

let%expect_test "behind schedule: submit the deficit, counting working" =
  (* At 10:16 the target is 1000*11/55 = 200. With 100 filled and 18 working,
     the deficit is 82. *)
  let parent = active ~deadline:"11:00:00" in
  let parent =
    Parent_order.add_child_exn parent (child ~id:1 ~quantity:100)
  in
  let parent =
    Parent_order.apply_fill_exn parent (fill ~order_id:1 ~size:100)
  in
  let parent =
    Parent_order.add_child_exn parent (child ~id:2 ~quantity:18)
  in
  actions ~now:"10:16:00" parent;
  [%expect
    {|
    ((Submit
      ((symbol NVDA) (side Buy) (quantity 82) (order_type Market)
       (time_in_force IOC))))
    |}]
;;

let%expect_test "ahead of schedule: stay quiet" =
  let parent = active ~deadline:"11:00:00" in
  let parent =
    Parent_order.add_child_exn parent (child ~id:1 ~quantity:250)
  in
  let parent =
    Parent_order.apply_fill_exn parent (fill ~order_id:1 ~size:250)
  in
  actions ~now:"10:16:00" parent;
  [%expect {| () |}]
;;

let%expect_test "at the deadline: submit exactly the remainder" =
  let parent = active ~deadline:"11:00:00" in
  let parent =
    Parent_order.add_child_exn parent (child ~id:1 ~quantity:600)
  in
  let parent =
    Parent_order.apply_fill_exn parent (fill ~order_id:1 ~size:600)
  in
  actions ~now:"11:00:00" parent;
  [%expect
    {|
    ((Submit
      ((symbol NVDA) (side Buy) (quantity 400) (order_type Market)
       (time_in_force IOC))))
    |}]
;;

let%expect_test "deadline = arrival means everything is due immediately" =
  actions ~now:"10:05:00" (active ~deadline:"10:05:00");
  [%expect
    {|
    ((Submit
      ((symbol NVDA) (side Buy) (quantity 1000) (order_type Market)
       (time_in_force IOC))))
    |}]
;;
