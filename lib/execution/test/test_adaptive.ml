open! Core
open! Execlab_types
open! Execlab_market
open Execlab_execution

(* Fixture: buy 1,000 NVDA between 10:05 and 11:00 — 55 minutes, so the
   schedule target at minute [m] is 1000 * m / 55, exactly as {!Twap}
   computes it, and the drift budget at [m] is round (patience * 0.25 *
   1000 * (55 - m) / 55). *)

let symbol = Symbol.of_string "NVDA"
let arrival_price = Price.of_int_cents 15_000

let instruction =
  Or_error.ok_exn
    (Alpha_instruction.create
       ~arrival_time:(Time_ns.Ofday.of_string "10:05:00")
       ~symbol
       ~side:Side.Buy
       ~quantity:(Size.of_int 1000)
       ~deadline:(Time_ns.Ofday.of_string "11:00:00"))
;;

let active () =
  Parent_order.activate_exn (Parent_order.create instruction) ~arrival_price
;;

(* A resting buy the algorithm might find on the book. *)
let resting ~id ~quantity ~cents =
  let request =
    Or_error.ok_exn
      (Child_order.Request.create
         ~symbol
         ~side:Side.Buy
         ~quantity:(Size.of_int quantity)
         ~order_type:(Order_type.Limit (Price.of_int_cents cents))
         ~time_in_force:Time_in_force.Day)
  in
  Child_order.create
    ~request
    ~id:(Order_id.For_testing.of_int id)
    ~submitted_at:(Time_ns.Ofday.of_string "10:06:00")
;;

let bar ~cents =
  Or_error.ok_exn
    (Market_bar.create
       ~time:(Time_ns.Ofday.of_string "10:05:00")
       ~open_:(Price.of_int_cents cents)
       ~high:(Price.of_int_cents cents)
       ~low:(Price.of_int_cents cents)
       ~close:(Price.of_int_cents cents)
       ~volume:(Size.of_int 10_000))
;;

(* The algorithm remembers the tape it last saw, so a test that means to
   exercise the damping has to walk it through consecutive bars: [steps] are
   fed in order and the actions of the last one are printed. *)
let actions ?(patience = 1.) ?(live_orders = []) steps =
  let (module A : Algorithm_intf.S) = Adaptive.create ~patience () in
  let parent = active () in
  let state = ref (A.init ~parent) in
  let latest = ref [] in
  List.iter steps ~f:(fun (now, last_cents) ->
    let context =
      { Algorithm_intf.Context.now = Time_ns.Ofday.of_string now
      ; previous_bar = bar ~cents:last_cents
      ; parent
      ; live_orders
      }
    in
    let next, actions = A.on_bar !state context in
    state := next;
    latest := actions);
  print_s [%sexp (!latest : Algorithm_intf.Action.t list)]
;;

(* Zero patience means a zero drift budget at every minute, so the passive
   branch is unreachable and every bar crosses for exactly what the schedule
   is owed — which is what {!Twap} submits, share for share. *)
let%expect_test "at zero patience this is twap: 1000 * 1 / 55 = 18, crossed" =
  actions ~patience:0. [ "10:06:00", 15_000 ];
  [%expect
    {|
    ((Submit
      ((symbol NVDA) (side Buy) (quantity 18) (order_type Market)
       (time_in_force IOC))))
    |}]
;;

(* A tape that has not moved since the last look costs nothing to wait at, so
   patience is undamped: round (0.25 * 1000 * 54/55) = 245 rest at the touch,
   and the 18 shares the schedule owes are inside that. *)
let%expect_test "a still market: the full drift budget rests at the touch" =
  actions [ "10:05:00", 15_000; "10:06:00", 15_000 ];
  [%expect
    {|
    ((Submit
      ((symbol NVDA) (side Buy) (quantity 245) (order_type (Limit 15000))
       (time_in_force Day))))
    |}]
;;

(* The same minute, but the tape moved 2 bps against us in the last bar — the
   damping half-life — which buys exactly half the patience: round (0.25 *
   0.5 * 1000 * 54/55) = 123. *)
let%expect_test "a market moving away at the half-life halves the budget" =
  actions [ "10:05:00", 15_000; "10:06:00", 15_003 ];
  [%expect
    {|
    ((Submit
      ((symbol NVDA) (side Buy) (quantity 123) (order_type (Limit 15003))
       (time_in_force Day))))
    |}]
;;

(* The tape at $149 is under the decision price. A price in front of the
   benchmark is worth crossing for, so the schedule's 18 shares go as a
   market order instead of resting. *)
let%expect_test "the tape is in front of the benchmark: cross for it" =
  actions [ "10:05:00", 15_000; "10:06:00", 14_900 ];
  [%expect
    {|
    ((Submit
      ((symbol NVDA) (side Buy) (quantity 18) (order_type Market)
       (time_in_force IOC))))
    |}]
;;

(* The budget tapers with the window: at 10:59 one minute is left, so round
   (0.25 * 1000 * 1/55) = 5 shares of drift are tolerable against a schedule
   owing 1000 * 54 / 55 = 981. Nothing rests; it all crosses. *)
let%expect_test "one minute left: the budget has tapered, so cross it all" =
  actions [ "10:58:00", 15_000; "10:59:00", 15_000 ];
  [%expect
    {|
    ((Submit
      ((symbol NVDA) (side Buy) (quantity 981) (order_type Market)
       (time_in_force IOC))))
    |}]
;;

(* An order already resting at the touch is left alone — cancelling it to
   post the same price again would only cost its place in the queue. 245 are
   wanted and 245 are resting, so there is nothing to do at all. *)
let%expect_test "a resting order at the right price keeps its queue place" =
  actions
    ~live_orders:[ resting ~id:1 ~quantity:245 ~cents:15_000 ]
    [ "10:05:00", 15_000; "10:06:00", 15_000 ];
  [%expect {| () |}]
;;

(* The touch has moved to $150.00, so an order resting at $149.90 is stale:
   pull it and rewrite the budget at the new price. *)
let%expect_test "the market moves past a resting order: reprice it" =
  actions
    ~live_orders:[ resting ~id:1 ~quantity:245 ~cents:14_990 ]
    [ "10:05:00", 15_000; "10:06:00", 15_000 ];
  [%expect
    {|
    ((Cancel 1)
     (Submit
      ((symbol NVDA) (side Buy) (quantity 245) (order_type (Limit 15000))
       (time_in_force Day))))
    |}]
;;

(* Crossing pulls everything first: the shares a passive order was sitting on
   are needed now, and leaving it live would let the parent's working total
   double-count them. *)
let%expect_test "crossing cancels the book first" =
  actions
    ~live_orders:[ resting ~id:1 ~quantity:245 ~cents:15_000 ]
    [ "10:58:00", 15_000; "10:59:00", 15_000 ];
  [%expect
    {|
    ((Cancel 1)
     (Submit
      ((symbol NVDA) (side Buy) (quantity 981) (order_type Market)
       (time_in_force IOC))))
    |}]
;;

(* At the arrival minute the schedule owes nothing and the window is whole,
   so the budget is at its widest and the entire first move is passive: round
   (0.25 * 1000 * 55/55) = 250. *)
let%expect_test "at arrival: nothing is owed, so the book is worked \
                 passively"
  =
  actions [ "10:05:00", 15_000 ];
  [%expect
    {|
    ((Submit
      ((symbol NVDA) (side Buy) (quantity 250) (order_type (Limit 15000))
       (time_in_force Day))))
    |}]
;;
