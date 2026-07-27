open! Core
open! Execlab_types

type t =
  { symbol : Symbol.t
  ; date : Date.t
  ; bars : Market_bar.t list
  }
[@@deriving sexp_of, compare, equal]

let bars_per_session = 390
let session_first_bar = Time_ns.Ofday.of_string "09:30:00"
let session_last_bar = Time_ns.Ofday.of_string "15:59:00"
let bar_time (bar : Market_bar.t) = bar.Market_bar.time

let create ~symbol ~date ~bars =
  let bar_count = List.length bars in
  if bar_count <> bars_per_session
  then
    Or_error.error_s
      [%message
        "Bars list must contain one bar per minute of the session"
          (bar_count : int)
          (bars_per_session : int)]
  else if not
            (List.is_sorted_strictly bars ~compare:(fun a b ->
               Time_ns.Ofday.compare (bar_time a) (bar_time b)))
  then
    Or_error.error_s
      [%message "Bars list must be sorted by strictly increasing time"]
  else (
    let first_bar_time = bar_time (List.hd_exn bars) in
    let last_bar_time = bar_time (List.last_exn bars) in
    if not (Time_ns.Ofday.equal first_bar_time session_first_bar)
    then
      Or_error.error_s
        [%message
          "First bar must be at the session open"
            (first_bar_time : Time_ns.Ofday.t)
            (session_first_bar : Time_ns.Ofday.t)]
    else if not (Time_ns.Ofday.equal last_bar_time session_last_bar)
    then
      Or_error.error_s
        [%message
          "Last bar must be at the final session minute"
            (last_bar_time : Time_ns.Ofday.t)
            (session_last_bar : Time_ns.Ofday.t)]
    else Ok { symbol; date; bars })
;;
