open! Core
open! Execlab_types
open! Execlab_market
open Execlab_execution

(* Fixture: buy 1,000 NVDA between 10:05 and 11:00. POV holds cumulative
   demand at [participation_rate * observed volume], where observed volume
   sums the completed bars at-or-after arrival. *)

let instruction =
  Or_error.ok_exn
    (Alpha_instruction.create
       ~arrival_time:(Time_ns.Ofday.of_string "10:05:00")
       ~symbol:(Symbol.of_string "NVDA")
       ~side:Side.Buy
       ~quantity:(Size.of_int 1000)
       ~deadline:(Time_ns.Ofday.of_string "11:00:00"))
;;

let active () =
  Parent_order.activate_exn
    (Parent_order.create instruction)
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

let bar ~time ~volume =
  Or_error.ok_exn
    (Market_bar.create
       ~time:(Time_ns.Ofday.of_string time)
       ~open_:(Price.of_int_cents 15000)
       ~high:(Price.of_int_cents 15000)
       ~low:(Price.of_int_cents 15000)
       ~close:(Price.of_int_cents 15000)
       ~volume:(Size.of_int volume))
;;

(* Runs one algorithm instance over [steps], threading POV's accumulated
   volume, and prints each step's actions. Each step supplies the parent as
   the order manager would present it at that minute. *)
let run ?min_child_size ?max_child_size ~participation_rate parent steps =
  let (module P : Algorithm_intf.S) =
    Pov.create ?min_child_size ?max_child_size ~participation_rate ()
  in
  let (_ : P.state) =
    List.fold
      steps
      ~init:(P.init ~parent)
      ~f:(fun state (now, previous_bar, parent) ->
        let context =
          { Algorithm_intf.Context.now = Time_ns.Ofday.of_string now
          ; previous_bar
          ; parent
          ; live_orders = Parent_order.live_children parent
          }
        in
        let state, actions = P.on_bar state context in
        print_s
          [%message (now : string) (actions : Algorithm_intf.Action.t list)];
        state)
  in
  ()
;;

let%expect_test "the pre-arrival bar does not count: no demand at arrival" =
  let parent = active () in
  run
    ~participation_rate:0.05
    parent
    [ "10:05:00", bar ~time:"10:04:00" ~volume:10000, parent ];
  [%expect {| ((now 10:05:00) (actions ())) |}]
;;

let%expect_test "one bar observed: demand 5% of it (4000 * 0.05 = 200)" =
  let parent = active () in
  run
    ~participation_rate:0.05
    parent
    [ "10:06:00", bar ~time:"10:05:00" ~volume:4000, parent ];
  [%expect
    {|
    ((now 10:06:00)
     (actions
      ((Submit
        ((symbol NVDA) (side Buy) (quantity 200) (order_type Market)
         (time_in_force IOC))))))
    |}]
;;

let%expect_test "volume accumulates; filled and working count as demanded" =
  (* Minute by minute at 5%: 10:06 sees bar 10:05 (4000 shares): observed
     4000, target 200 -> 200. 10:07 sees bar 10:06 (2500): observed 6500,
     target 325; 200 already filled -> 125. 10:08 sees bar 10:07 (100):
     observed 6600, target 330; 200 filled + 125 working -> 5. *)
  let at_10_06 = active () in
  let at_10_07 =
    Parent_order.apply_fill_exn
      (Parent_order.add_child_exn at_10_06 (child ~id:1 ~quantity:200))
      (fill ~order_id:1 ~size:200)
  in
  let at_10_08 =
    Parent_order.add_child_exn at_10_07 (child ~id:2 ~quantity:125)
  in
  run
    ~participation_rate:0.05
    at_10_06
    [ "10:06:00", bar ~time:"10:05:00" ~volume:4000, at_10_06
    ; "10:07:00", bar ~time:"10:06:00" ~volume:2500, at_10_07
    ; "10:08:00", bar ~time:"10:07:00" ~volume:100, at_10_08
    ];
  [%expect
    {|
    ((now 10:06:00)
     (actions
      ((Submit
        ((symbol NVDA) (side Buy) (quantity 200) (order_type Market)
         (time_in_force IOC))))))
    ((now 10:07:00)
     (actions
      ((Submit
        ((symbol NVDA) (side Buy) (quantity 125) (order_type Market)
         (time_in_force IOC))))))
    ((now 10:08:00)
     (actions
      ((Submit
        ((symbol NVDA) (side Buy) (quantity 5) (order_type Market)
         (time_in_force IOC))))))
    |}]
;;

let%expect_test "max child size caps the slice (target 5000 -> 300)" =
  let parent = active () in
  run
    ~participation_rate:0.5
    ~max_child_size:(Size.of_int 300)
    parent
    [ "10:06:00", bar ~time:"10:05:00" ~volume:10000, parent ];
  [%expect
    {|
    ((now 10:06:00)
     (actions
      ((Submit
        ((symbol NVDA) (side Buy) (quantity 300) (order_type Market)
         (time_in_force IOC))))))
    |}]
;;

let%expect_test "min child size holds back dust until the target grows" =
  (* At 1%: first bar's target is 30 < 50, so hold; after the second bar the
     target is 60 >= 50, and nothing was spoken for -> 60. *)
  let parent = active () in
  run
    ~participation_rate:0.01
    ~min_child_size:(Size.of_int 50)
    parent
    [ "10:06:00", bar ~time:"10:05:00" ~volume:3000, parent
    ; "10:07:00", bar ~time:"10:06:00" ~volume:3000, parent
    ];
  [%expect
    {|
    ((now 10:06:00) (actions ()))
    ((now 10:07:00)
     (actions
      ((Submit
        ((symbol NVDA) (side Buy) (quantity 60) (order_type Market)
         (time_in_force IOC))))))
    |}]
;;

let%expect_test "at the deadline, catch up: the whole remainder, rate be \
                 damned"
  =
  let parent =
    Parent_order.apply_fill_exn
      (Parent_order.add_child_exn (active ()) (child ~id:1 ~quantity:400))
      (fill ~order_id:1 ~size:400)
  in
  run
    ~participation_rate:0.05
    parent
    [ "11:00:00", bar ~time:"10:59:00" ~volume:100, parent ];
  [%expect
    {|
    ((now 11:00:00)
     (actions
      ((Submit
        ((symbol NVDA) (side Buy) (quantity 600) (order_type Market)
         (time_in_force IOC))))))
    |}]
;;

let%expect_test "the target never exceeds the parent's remaining" =
  let parent = active () in
  run
    ~participation_rate:1.0
    parent
    [ "10:06:00", bar ~time:"10:05:00" ~volume:50000, parent ];
  [%expect
    {|
    ((now 10:06:00)
     (actions
      ((Submit
        ((symbol NVDA) (side Buy) (quantity 1000) (order_type Market)
         (time_in_force IOC))))))
    |}]
;;
