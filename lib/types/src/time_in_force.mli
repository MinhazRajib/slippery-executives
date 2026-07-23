open! Core

type t =
  | Day (** Rests on the book until end of day *)
  | IOC (** Immediate or Cancel *)
[@@deriving sexp, bin_io, compare, equal, enumerate, hash, string]

val rests_on_book : t -> bool
val all_str : string
