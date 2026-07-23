(** A single alpha instruction - an instruction to buy or sell a given
    quantity of a symbol at a given time. *)

open! Core

type t = private
  { arrival_time : Time_ns.Ofday.t (** Anchors arrival-price slippage *)
  ; symbol : Symbol.t
  ; side : Side.t
  ; quantity : Size.t
  ; deadline : Time_ns.Ofday.t
  }
[@@deriving sexp, bin_io, compare, equal]

(** Creates a new alpha instruction. Validates arrival_time and deadline
    being within market hours. Also, makes sure deadline is after or equal to
    arrival_time. Makes sure size is positive. *)
val create
  :  arrival_time:Time_ns.Ofday.t
  -> symbol:Symbol.t
  -> side:Side.t
  -> quantity:Size.t
  -> deadline:Time_ns.Ofday.t
  -> t Or_error.t
