open! Core
open! Execlab_types
open! Execlab_market
open! Execlab_execution
open Execlab_simulation

(* A flat trading day: every bar opens at 150.00 with volume 42,000, so with
   the config pinned below the ask is 150.02, the bid 149.98, and the per-bar
   participation budget 4,200. TWAP slices (18-19 shares) carry ~0.2c of
   impact, which rounds to zero: they fill at 150.02 exactly.

   The config is pinned (not [Config.default]) so recalibrating the default
   impact coefficient doesn't churn these driver-semantics tests. *)

let fill_config : Fill_model.Config.t =
  { half_spread = Price.of_int_cents 2
  ; max_participation = 0.1
  ; impact_coefficient = Price.of_int_cents 10
  }
;;

let time_at_minute i =
  Option.value_exn
    (Time_ns.Ofday.add
       (Time_ns.Ofday.of_string "09:30:00")
       (Time_ns.Span.of_int_min i))
;;

let day =
  let price = Price.of_int_cents 15000 in
  let bars =
    List.init 390 ~f:(fun i ->
      Or_error.ok_exn
        (Market_bar.create
           ~time:(time_at_minute i)
           ~open_:price
           ~high:price
           ~low:price
           ~close:price
           ~volume:(Size.of_int 42000)))
  in
  Or_error.ok_exn
    (Trading_day.create
       ~symbol:(Symbol.of_string "NVDA")
       ~date:(Date.of_string "2026-07-09")
       ~bars)
;;

let instruction ~arrival ~deadline ~quantity =
  Or_error.ok_exn
    (Alpha_instruction.create
       ~arrival_time:(Time_ns.Ofday.of_string arrival)
       ~symbol:(Symbol.of_string "NVDA")
       ~side:Side.Buy
       ~quantity:(Size.of_int quantity)
       ~deadline:(Time_ns.Ofday.of_string deadline))
;;

let run ?(algorithm = (module Twap : Algorithm_intf.S)) instructions =
  Driver.run ~day ~instructions ~algorithm ~fill_config ()
;;

let summarize (result : Driver.t) =
  List.iter (Order_manager.parents result.manager) ~f:(fun p ->
    printf
      "parent: %s filled=%d\n"
      (Sexp.to_string [%sexp (p.status : Parent_order.Status.t)])
      (Size.to_int p.filled));
  let shares =
    List.sum (module Int) result.fills ~f:(fun f -> Size.to_int f.size)
  in
  let prices =
    List.map result.fills ~f:(fun f -> Price.to_int_cents f.price)
    |> List.dedup_and_sort ~compare:Int.compare
  in
  printf
    "fills=%d shares=%d distinct_prices=%s\n"
    (List.length result.fills)
    shares
    (Sexp.to_string [%sexp (prices : int list)])
;;

let%expect_test "twap completes a 55-minute parent in 55 slices at the ask" =
  summarize
    (run
       [ instruction ~arrival:"10:05:00" ~deadline:"11:00:00" ~quantity:1000
       ]);
  [%expect
    {|
    parent: Completed filled=1000
    fills=55 shares=1000 distinct_prices=(15002)
    |}]
;;

let%expect_test "deadline = arrival executes everything at once, paying \
                 more impact"
  =
  (* One 1,000-share market order: impact 10c * sqrt(1000/42000) = 1.5c ->
     2c, so it fills at 150.04 in a single fill. *)
  summarize
    (run
       [ instruction ~arrival:"10:05:00" ~deadline:"10:05:00" ~quantity:1000
       ]);
  [%expect
    {|
    parent: Completed filled=1000
    fills=1 shares=1000 distinct_prices=(15004)
    |}]
;;

let%expect_test "an impossible order hits the participation cap and expires" =
  (* 500,000 shares in two minutes: each minute fills only the 4,200 budget
     (impact 3c -> 150.05) and the IOC remainder is canceled; at 10:08 the
     parent expires with 8,400 done. *)
  summarize
    (run
       [ instruction
           ~arrival:"10:05:00"
           ~deadline:"10:07:00"
           ~quantity:500000
       ]);
  [%expect
    {|
    parent: Expired filled=8400
    fills=2 shares=8400 distinct_prices=(15005)
    |}]
;;

let%expect_test "two parents run side by side without interfering" =
  summarize
    (run
       [ instruction ~arrival:"10:05:00" ~deadline:"11:00:00" ~quantity:1000
       ; instruction ~arrival:"14:00:00" ~deadline:"15:00:00" ~quantity:500
       ]);
  [%expect
    {|
    parent: Completed filled=1000
    parent: Completed filled=500
    fills=115 shares=1500 distinct_prices=(15002)
    |}]
;;

let%expect_test "the immediate baseline pays for its impatience" =
  (* Same instruction TWAP completes at 150.02: Immediate slams the full
     1,000 in at once and pays 2c of impact -> one fill at 150.04. *)
  summarize
    (run
       ~algorithm:(module Immediate)
       [ instruction ~arrival:"10:05:00" ~deadline:"11:00:00" ~quantity:1000
       ]);
  [%expect
    {|
    parent: Completed filled=1000
    fills=1 shares=1000 distinct_prices=(15004)
    |}]
;;
