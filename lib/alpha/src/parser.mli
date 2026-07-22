open! Core
open! Execlab_types

type t = { instructions : list Alpha_instruction.t }
[@@deriving sexp, bin_io, compare, equal]


(*expected format:

*)
val parse : string -> (Alpha_instruction list, Error.t list) result