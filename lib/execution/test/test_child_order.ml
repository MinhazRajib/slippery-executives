open! Core
open! Execlab_types
open Execlab_execution

let request =
  Or_error.ok_exn
    (Child_order.Request.create
       ~symbol:(Symbol.of_string "NVDA")
       ~side:Side.Buy
       ~quantity:(Size.of_int 500)
       ~order_type:(Limit (Price.of_int_cents 15005))
       ~time_in_force:Time_in_force.Day)
;;

let order =
  Child_order.create
    ~request
    ~id:(Order_id.For_testing.of_int 1)
    ~submitted_at:(Time_ns.Ofday.of_string "10:05:00")
;;

let print t = print_s [%sexp (t : Child_order.t)]

let%expect_test "a fresh order is live with full remaining" =
  print order;
  [%expect
    {|
    ((id 1)
     (request
      ((symbol NVDA) (side Buy) (quantity 500) (order_type (Limit 15005))
       (time_in_force Day)))
     (submitted_at 10:05:00.000000000) (remaining 500) (status Live))
    |}]
;;

let%expect_test "partial fill decrements remaining and stays live" =
  let order = Child_order.apply_fill_exn order ~quantity:(Size.of_int 200) in
  printf
    "remaining %d, live %b\n"
    (Size.to_int order.remaining)
    (Child_order.is_live order);
  [%expect {| remaining 300, live true |}]
;;

let%expect_test "filling to zero flips status to Filled" =
  let order = Child_order.apply_fill_exn order ~quantity:(Size.of_int 500) in
  print_s [%sexp (order.status : Child_order.Status.t)];
  printf "live %b\n" (Child_order.is_live order);
  [%expect {|
    Filled
    live false
    |}]
;;

let%expect_test "overfill and zero-size fills raise" =
  Expect_test_helpers_core.require_does_raise (fun () ->
    Child_order.apply_fill_exn order ~quantity:(Size.of_int 501));
  [%expect
    {|
    ("Child_order.apply_fill_exn: invalid fill size"
     (quantity    501)
     (t.remaining 500))
    |}];
  Expect_test_helpers_core.require_does_raise (fun () ->
    Child_order.apply_fill_exn order ~quantity:Size.zero);
  [%expect
    {|
    ("Child_order.apply_fill_exn: invalid fill size"
     (quantity    0)
     (t.remaining 500))
    |}]
;;

let%expect_test "cancel keeps the unfilled size and kills the order" =
  let order =
    Child_order.cancel_exn order ~reason:Cancel_reason.Passive_timeout
  in
  print_s [%sexp (order.status : Child_order.Status.t)];
  printf
    "remaining %d, live %b\n"
    (Size.to_int order.remaining)
    (Child_order.is_live order);
  [%expect
    {|
    (Canceled Passive_timeout)
    remaining 500, live false
    |}]
;;

let%expect_test "operating on a dead order raises" =
  let canceled =
    Child_order.cancel_exn order ~reason:Cancel_reason.Algorithm_requested
  in
  Expect_test_helpers_core.require_does_raise (fun () ->
    Child_order.apply_fill_exn canceled ~quantity:(Size.of_int 100));
  [%expect
    {|
    ("Child_order: order is not live"
      (here apply_fill_exn)
      (t (
        (id 1)
        (request (
          (symbol   NVDA)
          (side     Buy)
          (quantity 500)
          (order_type (Limit 15005))
          (time_in_force Day)))
        (submitted_at 10:05:00.000000000)
        (remaining    500)
        (status (Canceled Algorithm_requested)))))
    |}];
  Expect_test_helpers_core.require_does_raise (fun () ->
    Child_order.cancel_exn canceled ~reason:Cancel_reason.Deadline_expired);
  [%expect
    {|
    ("Child_order: order is not live"
      (here cancel_exn)
      (t (
        (id 1)
        (request (
          (symbol   NVDA)
          (side     Buy)
          (quantity 500)
          (order_type (Limit 15005))
          (time_in_force Day)))
        (submitted_at 10:05:00.000000000)
        (remaining    500)
        (status (Canceled Algorithm_requested)))))
    |}]
;;

let%expect_test "a request must have positive quantity" =
  print_s
    [%sexp
      (Child_order.Request.create
         ~symbol:(Symbol.of_string "NVDA")
         ~side:Side.Sell
         ~quantity:Size.zero
         ~order_type:Order_type.Market
         ~time_in_force:Time_in_force.IOC
       : Child_order.Request.t Or_error.t)];
  [%expect {| (Error ("Quantity must be positive" (quantity 0))) |}]
;;
