open! Core
open! Execlab_types
open Execlab_execution

let instruction ~arrival ~quantity ~deadline =
  Or_error.ok_exn
    (Alpha_instruction.create
       ~arrival_time:(Time_ns.Ofday.of_string arrival)
       ~symbol:(Symbol.of_string "NVDA")
       ~side:Side.Buy
       ~quantity:(Size.of_int quantity)
       ~deadline:(Time_ns.Ofday.of_string deadline))
;;

let request quantity =
  Or_error.ok_exn
    (Child_order.Request.create
       ~symbol:(Symbol.of_string "NVDA")
       ~side:Side.Buy
       ~quantity:(Size.of_int quantity)
       ~order_type:Order_type.Market
       ~time_in_force:Time_in_force.IOC)
;;

let fill ~fill_id ~order_id ~size ~time =
  { Fill.fill_id
  ; symbol = Symbol.of_string "NVDA"
  ; price = Price.of_int_cents 15000
  ; size = Size.of_int size
  ; order_id = Order_id.For_testing.of_int order_id
  ; side = Side.Buy
  ; time = Time_ns.Ofday.of_string time
  ; liquidity = Liquidity.Taker
  }
;;

let manager =
  Order_manager.create
    ~instructions:
      [ instruction ~arrival:"10:05:00" ~quantity:1000 ~deadline:"10:30:00"
      ; instruction ~arrival:"10:10:00" ~quantity:500 ~deadline:"11:00:00"
      ]
;;

let price_for (_ : Symbol.t) = Price.of_int_cents 15000

let print_parents t =
  List.iteri (Order_manager.parents t) ~f:(fun i (p : Parent_order.t) ->
    printf
      "%d: %-9s filled=%-4d working=%-4d remaining=%d\n"
      i
      (Sexp.to_string [%sexp (p.status : Parent_order.Status.t)])
      (Size.to_int p.filled)
      (Size.to_int (Parent_order.working p))
      (Size.to_int (Parent_order.remaining p)))
;;

let%expect_test "activation sweeps only parents whose time has come" =
  print_parents manager;
  [%expect
    {|
    0: Pending   filled=0    working=0    remaining=1000
    1: Pending   filled=0    working=0    remaining=500
    |}];
  let t =
    Order_manager.activate_due
      manager
      ~now:(Time_ns.Ofday.of_string "10:05:00")
      ~price_for
  in
  print_parents t;
  [%expect
    {|
    0: Active    filled=0    working=0    remaining=1000
    1: Pending   filled=0    working=0    remaining=500
    |}];
  let t =
    Order_manager.activate_due
      t
      ~now:(Time_ns.Ofday.of_string "10:10:00")
      ~price_for
  in
  print_parents t;
  [%expect
    {|
    0: Active    filled=0    working=0    remaining=1000
    1: Active    filled=0    working=0    remaining=500
    |}]
;;

let%expect_test "a full session: submits, fills, cancels, expiry" =
  let now = Time_ns.Ofday.of_string "10:10:00" in
  let t = Order_manager.activate_due manager ~now ~price_for in
  (* ids are assigned sequentially across parents *)
  let t, child_1 =
    Order_manager.submit_exn t ~parent_index:0 ~request:(request 600) ~now
  in
  let t, child_2 =
    Order_manager.submit_exn t ~parent_index:1 ~request:(request 500) ~now
  in
  printf
    "child ids: %s %s\n"
    (Order_id.to_string child_1.id)
    (Order_id.to_string child_2.id);
  print_parents t;
  [%expect
    {|
    child ids: 1 2
    0: Active    filled=0    working=600  remaining=400
    1: Active    filled=0    working=500  remaining=0
    |}];
  let t =
    Order_manager.apply_fill_exn
      t
      (fill ~fill_id:1 ~order_id:1 ~size:600 ~time:"10:10:30")
  in
  let t =
    Order_manager.cancel_exn
      t
      ~order_id:child_2.id
      ~reason:Cancel_reason.Algorithm_requested
  in
  print_parents t;
  [%expect
    {|
    0: Active    filled=600  working=0    remaining=400
    1: Active    filled=0    working=0    remaining=500
    |}];
  (* submit another slice to parent 0, then let its 10:30 deadline pass *)
  let t, (_ : Child_order.t) =
    Order_manager.submit_exn t ~parent_index:0 ~request:(request 400) ~now
  in
  let t =
    Order_manager.expire_due t ~now:(Time_ns.Ofday.of_string "10:31:00")
  in
  print_parents t;
  [%expect
    {|
    0: Expired   filled=600  working=0    remaining=400
    1: Active    filled=0    working=0    remaining=500
    |}];
  let expired = Order_manager.parent_exn t 0 in
  print_s
    [%sexp ((List.hd_exn expired.children).status : Child_order.Status.t)];
  [%expect {| (Canceled Deadline_expired) |}]
;;

let%expect_test "the deadline minute itself may still trade" =
  let t =
    Order_manager.activate_due
      manager
      ~now:(Time_ns.Ofday.of_string "10:10:00")
      ~price_for
  in
  let t =
    Order_manager.expire_due t ~now:(Time_ns.Ofday.of_string "10:30:00")
  in
  print_parents t;
  [%expect
    {|
    0: Active    filled=0    working=0    remaining=1000
    1: Active    filled=0    working=0    remaining=500
    |}]
;;

let%expect_test "overfilling submits and unknown ids raise" =
  let now = Time_ns.Ofday.of_string "10:10:00" in
  let t = Order_manager.activate_due manager ~now ~price_for in
  Expect_test_helpers_core.show_raise (fun () ->
    Order_manager.submit_exn t ~parent_index:1 ~request:(request 501) ~now);
  [%expect
    {|
    (raised (
      "Parent_order.add_child_exn: would exceed parent quantity"
      (child_quantity 501)
      (filled         0)
      (working        0)
      (total          500)))
    |}];
  Expect_test_helpers_core.show_raise (fun () ->
    Order_manager.apply_fill_exn
      t
      (fill ~fill_id:1 ~order_id:99 ~size:1 ~time:"10:11:00"));
  [%expect
    {|
    (raised (
      "Order_manager: unknown order id"
      (here     apply_fill_exn)
      (order_id 99)))
    |}]
;;
