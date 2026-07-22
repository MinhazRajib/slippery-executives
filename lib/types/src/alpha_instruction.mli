open! Core

(** A single alpha instruction - an instruction to buy or sell a given
    quantity of a symbol at a given time. *)
type t =
  { arrival_time : Time_ns.Ofday.t
  ; symbol : Symbol.t
  ; side : Side.t
  ; quantity : int
  ; deadline : Time_ns.Ofday.t
  }
[@@deriving sexp, bin_io, compare, equal]

val create
  :  arrival_time:Time_ns.Ofday.t
  -> symbol:Symbol.t
  -> side:Side.t
  -> quantity:int
  -> deadline:Time_ns.Ofday.t
  -> t Or_error.t
