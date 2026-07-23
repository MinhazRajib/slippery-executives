open! Core

type t =
  | Market
  | Limit of Price.t
[@@deriving sexp, bin_io, compare, equal]

let to_string = function
  | Market -> "MKT"
  | Limit price -> [%string "LMT %{Price.to_string_dollar price}"]
;;
