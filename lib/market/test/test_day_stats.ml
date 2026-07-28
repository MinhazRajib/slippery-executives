open! Core
open! Execlab_types
open Execlab_market

let time_at_minute i =
  Option.value_exn
    (Time_ns.Ofday.add
       (Time_ns.Ofday.of_string "09:30:00")
       (Time_ns.Span.of_int_min i))
;;

let flat_bar_at_minute i ~price_cents ~volume =
  let price = Price.of_int_cents price_cents in
  Or_error.ok_exn
    (Market_bar.create
       ~time:(time_at_minute i)
       ~open_:price
       ~high:price
       ~low:price
       ~close:price
       ~volume:(Size.of_int volume))
;;

let day bars =
  Or_error.ok_exn
    (Trading_day.create
       ~symbol:(Symbol.of_string "TSLA")
       ~date:(Date.of_string "2026-07-09")
       ~bars)
;;

let uniform_day =
  day
    (List.init 390 ~f:(fun i ->
       flat_bar_at_minute i ~price_cents:39400 ~volume:1000))
;;

let%expect_test "uniform day: every stat has a closed-form answer" =
  printf
    "total_volume: %d\n"
    (Size.to_int (Day_stats.total_volume uniform_day));
  printf "vwap: %.2f\n" (Day_stats.vwap uniform_day);
  let profile = Day_stats.volume_profile uniform_day in
  printf "profile entries: %d\n" (List.length profile);
  printf "first entry: %.6f\n" (List.hd_exn profile);
  printf "sum: %.6f\n" (List.sum (module Float) profile ~f:Fn.id);
  printf "volatility: %.6f\n" (Day_stats.realized_volatility uniform_day);
  [%expect
    {|
    total_volume: 390000
    vwap: 394.00
    profile entries: 390
    first entry: 0.002564
    sum: 1.000000
    volatility: 0.000000
    |}]
;;

let%expect_test "two-regime day: vwap is volume-weighted, not a simple \
                 average"
  =
  (* 195 bars at $100 x 1000 shares, then 195 bars at $200 x 3000 shares.
     Three quarters of the shares traded at $200, so the volume-weighted
     average is $175 -- not the $150 midpoint. *)
  let two_regime =
    day
      (List.init 390 ~f:(fun i ->
         if i < 195
         then flat_bar_at_minute i ~price_cents:10000 ~volume:1000
         else flat_bar_at_minute i ~price_cents:20000 ~volume:3000))
  in
  printf "vwap: %.2f\n" (Day_stats.vwap two_regime);
  let profile = Day_stats.volume_profile two_regime in
  printf "first entry: %.6f\n" (List.hd_exn profile);
  printf "last entry: %.6f\n" (List.last_exn profile);
  [%expect
    {|
    vwap: 175.00
    first entry: 0.001282
    last entry: 0.003846
    |}]
;;

let%expect_test "alternating closes produce nonzero volatility" =
  (* Closes alternate $100 / $101, so minute log returns alternate +/-
     log(1.01) ~ 0.00995; daily volatility ~ 0.00995 * sqrt(390). *)
  let alternating =
    day
      (List.init 390 ~f:(fun i ->
         let price_cents = if i % 2 = 0 then 10000 else 10100 in
         flat_bar_at_minute i ~price_cents ~volume:1000))
  in
  printf "volatility: %.4f\n" (Day_stats.realized_volatility alternating);
  [%expect {| volatility: 0.1968 |}]
;;
