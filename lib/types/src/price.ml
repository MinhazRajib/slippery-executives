open! Core

module T = struct
  type t = int [@@deriving sexp, bin_io, compare, equal, hash]
end

include T
include Comparable.Make (T)

let cents_per_dollar = 100
let of_int_cents n = n
let to_int_cents t = t

(* Exact-cent floats don't scale to exact integers (1.15 *. 100. =
   114.99999999999999), so we accept anything within [tolerance] of a whole
   number of cents. Representation noise is ~1e-8 cents at realistic prices;
   genuine sub-cent inputs are off by >= 0.1 cents. *)
let tolerance = 1e-6

let of_float_exn f =
  let scaled = f *. Float.of_int cents_per_dollar in
  let cents = Float.round_nearest scaled in
  if Float.( > ) (Float.abs (scaled -. cents)) tolerance
  then
    raise_s
      [%message
        "Price.of_float_exn: not representable as exact cents" (f : float)];
  (* [Float.to_int] raises on NaN and out-of-range inputs. *)
  Float.to_int cents
;;

let to_float t = Float.of_int t /. Float.of_int cents_per_dollar
let zero = 0
let ( + ) = Int.( + )
let ( - ) = Int.( - )
let ( * ) price qty = price * qty

let is_more_aggressive (side : Side.t) ~price ~than =
  match side with Buy -> price > than | Sell -> price < than
;;

let is_marketable (side : Side.t) ~price ~resting_price =
  match side with
  | Buy -> price >= resting_price
  | Sell -> price <= resting_price
;;

let to_string_dollar t =
  let is_negative = t < 0 in
  let t_abs = Int.abs t in
  let dollars = t_abs / cents_per_dollar in
  let cents = t_abs mod cents_per_dollar in
  sprintf "%s$%d.%02d" (if is_negative then "-" else "") dollars cents
;;

let to_string = to_string_dollar

let of_string s =
  let s = String.chop_prefix_if_exists s ~prefix:"$" in
  of_float_exn (Float.of_string s)
;;

(* [Float.to_int] raises on NaN and out-of-range inputs, so no extra guard is
   needed here. *)
let of_float_round_nearest f =
  Float.to_int (Float.round_nearest (f *. Float.of_int cents_per_dollar))
;;
