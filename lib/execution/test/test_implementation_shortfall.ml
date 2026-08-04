open! Core
open! Execlab_types
open! Execlab_market
open Execlab_execution

(* Fixture: buy 1,000 NVDA between 10:05 and 11:00 (55 minutes). With urgency
   u, the scheduled remaining quantity at elapsed fraction f is 1000 *
   sinh(u * (1 - f)) / sinh(u), so the target is 1000 minus that, rounded to
   nearest. Hand-computed for u = 2:

   {v
     minute  f       remaining               target   (TWAP would say)
     0       0       1000.00                    0        0
     1       1/55     962.93                   37       18
     11      0.2      654.99                  345      200
     55      1        sinh(0) = 0            1000     1000
   v}

   u -> 0 recovers the straight line (at minute 11: 200); huge urgency is
   risk-dominated: at u = 50, minute 1, remaining is about 1000 * e^(-50/55)
   = 403, so the target is already 597. *)

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

let actions ?(urgency = 2.0) ~now parent =
  let module Algo = (val Implementation_shortfall.create ~urgency ()) in
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
  actions ~now:"10:05:00" (active ~deadline:"11:00:00");
  [%expect {| () |}]
;;

let%expect_test "one minute in: the front-loaded first slice is 37 where \
                 TWAP would send 18"
  =
  actions ~now:"10:06:00" (active ~deadline:"11:00:00");
  [%expect
    {|
    ((Submit
      ((symbol NVDA) (side Buy) (quantity 37) (order_type Market)
       (time_in_force IOC))))
    |}]
;;

let%expect_test "behind schedule at 10:16: target 345, minus 100 filled and \
                 18 working, is a 227 deficit"
  =
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
      ((symbol NVDA) (side Buy) (quantity 227) (order_type Market)
       (time_in_force IOC))))
    |}]
;;

let%expect_test "ahead of schedule: stay quiet" =
  let parent = active ~deadline:"11:00:00" in
  let parent =
    Parent_order.add_child_exn parent (child ~id:1 ~quantity:400)
  in
  let parent =
    Parent_order.apply_fill_exn parent (fill ~order_id:1 ~size:400)
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

let%expect_test "zero urgency is the straight line: TWAP's 200 at 10:16" =
  actions ~urgency:0. ~now:"10:16:00" (active ~deadline:"11:00:00");
  [%expect
    {|
    ((Submit
      ((symbol NVDA) (side Buy) (quantity 200) (order_type Market)
       (time_in_force IOC))))
    |}]
;;

let%expect_test "huge urgency is risk-dominated: 597 due after one minute" =
  actions ~urgency:50. ~now:"10:06:00" (active ~deadline:"11:00:00");
  [%expect
    {|
    ((Submit
      ((symbol NVDA) (side Buy) (quantity 597) (order_type Market)
       (time_in_force IOC))))
    |}]
;;

let%expect_test "a zero-length window is all due immediately" =
  actions ~now:"10:05:00" (active ~deadline:"10:05:00");
  [%expect
    {|
    ((Submit
      ((symbol NVDA) (side Buy) (quantity 1000) (order_type Market)
       (time_in_force IOC))))
    |}]
;;

let%expect_test "urgency far beyond sinh's overflow point stays finite: \
                 everything due after one minute"
  =
  (* Naive sinh overflows to inf at u ~ 711; the exponential form keeps the
     ratio finite. At u = 1000, remaining after one minute is 1000 *
     e^(-1000/55) ~ 1.3e-5, which rounds to 0: target 1000. *)
  actions ~urgency:1000. ~now:"10:06:00" (active ~deadline:"11:00:00");
  [%expect
    {|
    ((Submit
      ((symbol NVDA) (side Buy) (quantity 1000) (order_type Market)
       (time_in_force IOC))))
    |}]
;;

let%expect_test "zero urgency agrees with twap share for share, every \
                 minute of the window"
  =
  (* Not merely the same shape: the same integer arithmetic. Rounding the
     curve to nearest — what every other urgency does — parts company with
     Twap's truncation whenever the ideal position lands past the half, which
     is most minutes of a 55-minute window. *)
  let quantity_of module_actions =
    match (module_actions : Algorithm_intf.Action.t list) with
    | [ Submit request ] -> Size.to_int request.Child_order.Request.quantity
    | [] -> 0
    | (_ : Algorithm_intf.Action.t list) -> -1
  in
  let at ~now =
    let parent = active ~deadline:"11:00:00" in
    let context =
      { Algorithm_intf.Context.now = Time_ns.Ofday.of_string now
      ; previous_bar
      ; parent
      ; live_orders = Parent_order.live_children parent
      }
    in
    let module Is = (val Implementation_shortfall.create ~urgency:0. ()) in
    let (_ : Is.state), is_actions = Is.on_bar (Is.init ~parent) context in
    let (_ : Twap.state), twap_actions =
      Twap.on_bar (Twap.init ~parent) context
    in
    quantity_of is_actions, quantity_of twap_actions
  in
  let disagreements =
    List.filter_map (List.range 0 56) ~f:(fun minute ->
      let now =
        sprintf "%02d:%02d:00" (10 + ((5 + minute) / 60)) ((5 + minute) % 60)
      in
      let is, twap = at ~now in
      if is = twap then None else Some (now, is, twap))
  in
  print_s [%sexp (disagreements : (string * int * int) list)];
  [%expect {| () |}]
;;
