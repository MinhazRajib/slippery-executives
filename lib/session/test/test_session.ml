open! Core
open! Execlab_types
open! Execlab_market
open Execlab_session

(* A flat $150.00 day (volume 42,000/bar), default config. TWAP slices ~18
   shares: impact 25c * sqrt(18/42000) = 0.52c, which rounds to 1c a share --
   1,000c across the order -- plus the 2c spread on every share (2,000c):
   friction 3,000c, no drift on a flat tape. Immediate's one 1,000-share fill
   pays 25c * sqrt(1000/42000) = 3.86c -> 4c a share (4,000c) plus the same
   spread: friction 6,000c. Value add of slicing = 6,000 - 3,000 = 3,000c,
   pure impact difference. *)

let day =
  let price = Price.of_int_cents 15000 in
  let bar_at_minute i =
    Or_error.ok_exn
      (Market_bar.create
         ~time:
           (Option.value_exn
              (Time_ns.Ofday.add
                 (Time_ns.Ofday.of_string "09:30:00")
                 (Time_ns.Span.of_int_min i)))
         ~open_:price
         ~high:price
         ~low:price
         ~close:price
         ~volume:(Size.of_int 42000))
  in
  Or_error.ok_exn
    (Trading_day.create
       ~symbol:(Symbol.of_string "NVDA")
       ~date:(Date.of_string "2026-07-09")
       ~bars:(List.init 390 ~f:bar_at_minute))
;;

let instructions =
  [ Or_error.ok_exn
      (Alpha_instruction.create
         ~arrival_time:(Time_ns.Ofday.of_string "10:05:00")
         ~symbol:(Symbol.of_string "NVDA")
         ~side:Side.Buy
         ~quantity:(Size.of_int 1000)
         ~deadline:(Time_ns.Ofday.of_string "11:00:00"))
  ]
;;

let%expect_test "twap on a flat day: all spread, no impact, no drift" =
  let outcome =
    Or_error.ok_exn
      (run
         ~universe:(Universe.of_day day)
         ~forecast_days:Symbol.Map.empty
         ~instructions
         ~algo_name:"twap"
         ~params:Params.default)
  in
  let graded = List.hd_exn outcome.graded in
  let grading = graded.Graded.grading in
  printf
    "friction %d = timing %d + spread %d + impact %d\n"
    grading.friction_cost_cents
    grading.timing_cost_cents
    grading.spread_cost_cents
    grading.impact_cost_cents;
  printf "value add vs immediate: %d\n" graded.value_add_cents;
  printf
    "identity: %b\n"
    (Outcome.gross_cents outcome
     = Outcome.net_cents outcome
       + Outcome.shortfall_cents outcome
       + List.sum (module Int) outcome.graded ~f:(fun g ->
         g.Graded.grading.opportunity_cost_cents));
  [%expect
    {|
    friction 3000 = timing 0 + spread 2000 + impact 1000
    value add vs immediate: 3000
    identity: true
    |}]
;;

let%expect_test "an unknown algorithm is an error, not an exception" =
  print_s
    [%sexp
      (Or_error.ignore_m
         (run
            ~universe:(Universe.of_day day)
            ~forecast_days:Symbol.Map.empty
            ~instructions
            ~algo_name:"guerrilla"
            ~params:Params.default)
       : unit Or_error.t)];
  [%expect
    {|
    (Error
     ("unknown algorithm" (other guerrilla)
      (known "twap, vwap, pov, is, immediate")))
    |}]
;;

let%expect_test "a synthetic run attributes no configured spread: the \
                 residual is all impact, and the identity still holds"
  =
  let outcome =
    Or_error.ok_exn
      (run
         ~universe:(Universe.of_day day)
         ~forecast_days:Symbol.Map.empty
         ~instructions
         ~algo_name:"twap"
         ~params:
           { Params.default with
             engine = Engine_choice.Synthetic { seed = 1 }
           })
  in
  let grading = (List.hd_exn outcome.graded).Graded.grading in
  printf "spread: %d\n" grading.spread_cost_cents;
  printf "impact at least 0: %b\n" (grading.impact_cost_cents >= 0);
  printf
    "splits exactly: %b\n"
    (grading.friction_cost_cents
     = grading.timing_cost_cents
       + grading.spread_cost_cents
       + grading.impact_cost_cents);
  [%expect
    {|
    spread: 0
    impact at least 0: true
    splits exactly: true
    |}]
;;

let%expect_test "instructions for another symbol are rejected, whichever \
                 front submits them"
  =
  let foreign =
    [ Or_error.ok_exn
        (Alpha_instruction.create
           ~arrival_time:(Time_ns.Ofday.of_string "10:05:00")
           ~symbol:(Symbol.of_string "TSLA")
           ~side:Side.Buy
           ~quantity:(Size.of_int 100)
           ~deadline:(Time_ns.Ofday.of_string "11:00:00"))
    ]
  in
  print_s
    [%sexp
      (Or_error.ignore_m
         (run
            ~universe:(Universe.of_day day)
            ~forecast_days:Symbol.Map.empty
            ~instructions:foreign
            ~algo_name:"twap"
            ~params:Params.default)
       : unit Or_error.t)];
  [%expect
    {|
    (Error
     ("the alpha names a symbol this run has no session for" (symbol TSLA)
      (loaded (NVDA))))
    |}]
;;
