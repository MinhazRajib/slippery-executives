open! Core
open! Execlab_types

type t = { instructions : Alpha_instruction.t list }
[@@deriving sexp, bin_io, compare, equal]

(*expected format:
    arrival_time,symbol,side,quantity,deadline
    09:30:00,AAPL,BUY,100,10:15:00]
*)

val parse : string -> t Or_error.t
