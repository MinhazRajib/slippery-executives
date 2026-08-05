open! Core
open! Execlab_types
open! Execlab_market
open Execlab_execution

(* Fixture: buy 1,000 NVDA between 10:05 and 10:10. The profile weight inside
   the window sums to 0.10, so the schedule target at time [now] is 1000 *
   (weight accumulated since 10:05) / 0.10:

   {v
     now     in-window weight so far   target
     10:05   0.00                         0
     10:06   0.04                       400     (TWAP would say 200)
     10:07   0.05                       500
     10:08   0.07                       700
     10:09   0.09                       900
     10:10   0.10                      1000
   v}

   The 10:04 and 10:10 entries are decoys: bars before arrival or at the
   deadline must not shape the schedule. If 10:10 leaked into the window, the
   deadline target would be 1000 * 0.10 / 0.15 = 667, not 1000. *)

let profile =
  List.map
    [ "10:04:00", 0.05 (* before arrival: ignored *)
    ; "10:05:00", 0.04
    ; "10:06:00", 0.01
    ; "10:07:00", 0.02
    ; "10:08:00", 0.02
    ; "10:09:00", 0.01
    ; "10:10:00", 0.05 (* at the deadline: excluded *)
    ]
    ~f:(fun (time, weight) -> Time_ns.Ofday.of_string time, weight)
;;

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

let actions ?(profile = profile) ~now parent =
  let module Algo =
    (val Vwap.create
           ~profiles:(Symbol.Map.singleton (Symbol.of_string "NVDA") profile))
  in
  let context =
    { Algorithm_intf.Context.now = Time_ns.Ofday.of_string now
    ; previous_bar
    ; parent
    ; live_orders = Parent_order.live_children parent
    }
  in
  let (_ : Algo.state), actions = Algo.on_bar (Algo.init ~parent) context in
  print_s [%sexp (actions : Algorithm_intf.Action.t list)]
;;

let%expect_test "at arrival the target is zero: no action" =
  actions ~now:"10:05:00" (active ~deadline:"10:10:00");
  [%expect {| () |}]
;;

let%expect_test "one minute in: the busy 10:05 bar puts the slice at 400" =
  actions ~now:"10:06:00" (active ~deadline:"10:10:00");
  [%expect
    {|
    ((Submit
      ((symbol NVDA) (side Buy) (quantity 400) (order_type Market)
       (time_in_force IOC))))
    |}]
;;

let%expect_test "behind schedule: submit the deficit, counting working" =
  (* At 10:07 the target is 1000 * 0.05 / 0.10 = 500. With 100 filled and 18
     working, the deficit is 382. *)
  let parent = active ~deadline:"10:10:00" in
  let parent =
    Parent_order.add_child_exn parent (child ~id:1 ~quantity:100)
  in
  let parent =
    Parent_order.apply_fill_exn parent (fill ~order_id:1 ~size:100)
  in
  let parent =
    Parent_order.add_child_exn parent (child ~id:2 ~quantity:18)
  in
  actions ~now:"10:07:00" parent;
  [%expect
    {|
    ((Submit
      ((symbol NVDA) (side Buy) (quantity 382) (order_type Market)
       (time_in_force IOC))))
    |}]
;;

let%expect_test "ahead of schedule: stay quiet" =
  (* 600 filled at 10:07 is ahead of the 500 target. *)
  let parent = active ~deadline:"10:10:00" in
  let parent =
    Parent_order.add_child_exn parent (child ~id:1 ~quantity:600)
  in
  let parent =
    Parent_order.apply_fill_exn parent (fill ~order_id:1 ~size:600)
  in
  actions ~now:"10:07:00" parent;
  [%expect {| () |}]
;;

let%expect_test "at the deadline: submit exactly the remainder" =
  let parent = active ~deadline:"10:10:00" in
  let parent =
    Parent_order.add_child_exn parent (child ~id:1 ~quantity:600)
  in
  let parent =
    Parent_order.apply_fill_exn parent (fill ~order_id:1 ~size:600)
  in
  actions ~now:"10:10:00" parent;
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

let%expect_test "a symbol with no forecast walks Twap's straight line" =
  (* A run may touch a symbol we have no other sessions for, and there is
     then nothing to shape a schedule with. Degrading to the straight line is
     the honest answer; degrading to a market order — which is what a zero
     window weight used to mean — would have turned a patient algorithm into
     an impatient one precisely when it knew least. Over a five-minute
     window: nothing due at arrival, 200 a minute later. *)
  actions ~profile:[] ~now:"10:05:00" (active ~deadline:"10:10:00");
  actions ~profile:[] ~now:"10:06:00" (active ~deadline:"10:10:00");
  [%expect
    {|
    ()
    ((Submit
      ((symbol NVDA) (side Buy) (quantity 200) (order_type Market)
       (time_in_force IOC))))
    |}]
;;
