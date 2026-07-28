open! Core
open! Execlab_types
open! Execlab_market

type t =
  | Algorithm_requested
  | Passive_timeout
  | Deadline_expired
  | End_of_day
[@@deriving sexp, bin_io, compare, equal, enumerate, hash, string]
