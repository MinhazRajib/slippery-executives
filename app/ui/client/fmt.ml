(* Human formatting shared by every screen: dollars, basis points,
   percentages, share counts, times of day. String producers only — no vdom
   here. *)

open! Core
open! Execlab_types

let shares_int n = Int.to_string_hum ~delimiter:',' n
let shares size = shares_int (Size.to_int size)

(* [$1,234.56] — magnitude only; use {!signed_cents} when the sign is the
   point. *)
let cents_magnitude cents =
  let magnitude = abs cents in
  let cents_part = sprintf "%02d" (magnitude mod 100) in
  [%string "$%{shares_int (magnitude / 100)}.%{cents_part}"]
;;

let cents cents_value =
  let sign = if cents_value < 0 then "-" else "" in
  [%string "%{sign}%{cents_magnitude cents_value}"]
;;

(* [+$12.34] / [-$12.34] — for P&L-like quantities where sign carries the
   verdict. *)
let signed_cents cents_value =
  let sign = if cents_value < 0 then "-" else "+" in
  [%string "%{sign}%{cents_magnitude cents_value}"]
;;

let price p = Price.to_string_dollar p
let dollars_4dp value = sprintf "$%.4f" value
let bps value = sprintf "%+.1f" value
let pct value = sprintf "%.1f%%" (value *. 100.)

let ofday t =
  let seconds =
    Float.to_int
      (Time_ns.Span.to_sec (Time_ns.Ofday.to_span_since_start_of_day t))
  in
  sprintf "%02d:%02d" (seconds / 3600) (seconds mod 3600 / 60)
;;

let date d = Date.to_string d
