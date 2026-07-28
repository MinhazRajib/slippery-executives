open! Core
open! Execlab_types

let total_volume (day : Trading_day.t) =
  List.fold day.Trading_day.bars ~init:Size.zero ~f:(fun acc bar ->
    Size.( + ) acc bar.Market_bar.volume)
;;

let typical_price (bar : Market_bar.t) =
  (Price.to_float bar.high
   +. Price.to_float bar.low
   +. Price.to_float bar.close)
  /. 3.
;;

let nonzero_total_volume_exn (day : Trading_day.t) =
  let total = Size.to_float (total_volume day) in
  if Float.( = ) total 0.
  then raise_s [%message "Day_stats: day has zero total volume"];
  total
;;

let vwap (day : Trading_day.t) =
  let total = nonzero_total_volume_exn day in
  let dollar_volume =
    List.sum (module Float) day.bars ~f:(fun bar ->
      typical_price bar *. Size.to_float bar.Market_bar.volume)
  in
  dollar_volume /. total
;;

let volume_profile (day : Trading_day.t) =
  let total = nonzero_total_volume_exn day in
  List.map day.bars ~f:(fun bar ->
    Size.to_float bar.Market_bar.volume /. total)
;;

let realized_volatility (day : Trading_day.t) =
  let closes =
    List.map day.bars ~f:(fun bar -> Price.to_float bar.Market_bar.close)
  in
  let returns =
    List.map2_exn
      (List.drop_last_exn closes)
      (List.drop closes 1)
      ~f:(fun previous current -> Float.log (current /. previous))
  in
  let n = Float.of_int (List.length returns) in
  let mean = List.sum (module Float) returns ~f:Fn.id /. n in
  let variance =
    List.sum (module Float) returns ~f:(fun return ->
      Float.square (return -. mean))
    /. (n -. 1.)
  in
  Float.sqrt variance *. Float.sqrt 390.
;;
