open! Core
open! Execlab_types
open Execlab_execution

let instruction =
  Or_error.ok_exn
    (Alpha_instruction.create
       ~arrival_time:(Time_ns.Ofday.of_string "10:05:00")
       ~symbol:(Symbol.of_string "NVDA")
       ~side:Side.Buy
       ~quantity:(Size.of_int 1000)
       ~deadline:(Time_ns.Ofday.of_string "11:00:00"))
;;

let child ?(symbol = "NVDA") ~id ~quantity () =
  let request =
    Or_error.ok_exn
      (Child_order.Request.create
         ~symbol:(Symbol.of_string symbol)
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

let fill ~fill_id ~order_id ~size =
  { Fill.fill_id
  ; symbol = Symbol.of_string "NVDA"
  ; price = Price.of_int_cents 15000
  ; size = Size.of_int size
  ; order_id = Order_id.For_testing.of_int order_id
  ; side = Side.Buy
  ; time = Time_ns.Ofday.of_string "10:06:30"
  ; liquidity = Liquidity.Taker
  }
;;

let summary (t : Parent_order.t) =
  print_s
    [%message
      ""
        ~status:(t.status : Parent_order.Status.t)
        ~arrival_price:(t.arrival_price : Price.t option)
        ~filled:(t.filled : Size.t)
        ~working:(Parent_order.working t : Size.t)
        ~remaining:(Parent_order.remaining t : Size.t)]
;;

let pending = Parent_order.create instruction

let active =
  Parent_order.activate_exn pending ~arrival_price:(Price.of_int_cents 15000)
;;

let%expect_test "a fresh parent is pending with no benchmark" =
  summary pending;
  [%expect
    {| ((status Pending) (arrival_price ()) (filled 0) (working 0) (remaining 1000)) |}]
;;

let%expect_test "activation samples the arrival price, exactly once" =
  summary active;
  [%expect
    {|
    ((status Active) (arrival_price (15000)) (filled 0) (working 0)
     (remaining 1000))
    |}];
  Expect_test_helpers_core.require_does_raise (fun () ->
    Parent_order.activate_exn active ~arrival_price:(Price.of_int_cents 1));
  [%expect
    {|
    ("Parent_order: unexpected status"
      (here     activate_exn)
      (expected Pending)
      (actual   Active))
    |}]
;;

let%expect_test "children and fills move both ledgers" =
  let parent =
    Parent_order.add_child_exn active (child ~id:1 ~quantity:600 ())
  in
  summary parent;
  [%expect
    {|
    ((status Active) (arrival_price (15000)) (filled 0) (working 600)
     (remaining 400))
    |}];
  Expect_test_helpers_core.require_does_raise (fun () ->
    Parent_order.add_child_exn parent (child ~id:2 ~quantity:500 ()));
  [%expect
    {|
    ("Parent_order.add_child_exn: would exceed parent quantity"
     (child_quantity 500)
     (filled         0)
     (working        600)
     (total          1000))
    |}];
  let parent =
    Parent_order.apply_fill_exn
      parent
      (fill ~fill_id:1 ~order_id:1 ~size:600)
  in
  summary parent;
  [%expect
    {|
    ((status Active) (arrival_price (15000)) (filled 600) (working 0)
     (remaining 400))
    |}];
  let parent =
    Parent_order.add_child_exn parent (child ~id:2 ~quantity:400 ())
  in
  let parent =
    Parent_order.apply_fill_exn
      parent
      (fill ~fill_id:2 ~order_id:2 ~size:400)
  in
  summary parent;
  [%expect
    {|
    ((status Completed) (arrival_price (15000)) (filled 1000) (working 0)
     (remaining 0))
    |}]
;;

let%expect_test "a fill for an unknown order raises" =
  Expect_test_helpers_core.require_does_raise (fun () ->
    Parent_order.apply_fill_exn active (fill ~fill_id:1 ~order_id:99 ~size:1));
  [%expect
    {|
    ("Parent_order.apply_fill_exn: unknown order id"
     (fill (
       (fill_id   1)
       (symbol    NVDA)
       (price     15000)
       (size      1)
       (order_id  99)
       (side      Buy)
       (time      10:06:30.000000000)
       (liquidity Taker))))
    |}]
;;

let%expect_test "a child for the wrong symbol raises" =
  Expect_test_helpers_core.require_does_raise (fun () ->
    Parent_order.add_child_exn
      active
      (child ~symbol:"AMD" ~id:1 ~quantity:100 ()));
  [%expect
    {|
    ("Parent_order.add_child_exn: child symbol does not match instruction"
     (child_symbol       AMD)
     (instruction_symbol NVDA))
    |}]
;;

let%expect_test "expiry cancels live children and freezes the parent" =
  let parent =
    Parent_order.add_child_exn active (child ~id:3 ~quantity:250 ())
  in
  let parent = Parent_order.expire_exn parent in
  summary parent;
  print_s
    [%sexp ((List.hd_exn parent.children).status : Child_order.Status.t)];
  [%expect
    {|
    ((status Expired) (arrival_price (15000)) (filled 0) (working 0)
     (remaining 1000))
    (Canceled Deadline_expired)
    |}];
  Expect_test_helpers_core.require_does_raise (fun () ->
    Parent_order.apply_fill_exn parent (fill ~fill_id:1 ~order_id:3 ~size:1));
  [%expect
    {|
    ("Parent_order: unexpected status"
      (here     apply_fill_exn)
      (expected Active)
      (actual   Expired))
    |}];
  Expect_test_helpers_core.require_does_raise (fun () ->
    Parent_order.add_child_exn pending (child ~id:4 ~quantity:100 ()));
  [%expect
    {|
    ("Parent_order: unexpected status"
      (here     add_child_exn)
      (expected Active)
      (actual   Pending))
    |}]
;;
