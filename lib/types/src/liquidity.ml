open! Core

type t =
  | Taker
  | Maker
[@@deriving sexp, bin_io, compare, equal, enumerate, hash, string]
