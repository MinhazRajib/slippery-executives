open! Core
open! Execlab_types
open! Execlab_market

let arrival_price (day : Trading_day.t) ~arrival_time =
  match
    List.find day.bars ~f:(fun (bar : Market_bar.t) ->
      Time_ns.Ofday.( >= ) bar.time arrival_time)
  with
  | Some bar -> Ok bar.open_
  | None ->
    Or_error.error_s
      [%message
        "Benchmarks.arrival_price: no session minute at or after arrival"
          ~symbol:(day.symbol : Symbol.t)
          (arrival_time : Time_ns.Ofday.t)]
;;

(* [Trading_day.create] guarantees exactly 390 bars, so the session always
   has a final minute. *)
let terminal_price (day : Trading_day.t) = (List.last_exn day.bars).close
