(** One symbol's full regular trading session: minute-by-minute bars for a
    specific date. [create] validates the sequence-level invariants, so a [t]
    always holds exactly one bar per session minute, sorted by strictly
    increasing time from 09:30:00 through 15:59:00. *)

open! Core
open! Execlab_types

type t = private
  { symbol : Symbol.t
  ; date : Date.t
  ; bars : Market_bar.t list
  }
[@@deriving sexp_of, compare, equal]

(** validates sequence level invariants - ascending strictly-unique times,
    expected count (390) first bar at 9:30 last bar at 15:59 *)
val create
  :  symbol:Symbol.t
  -> date:Date.t
  -> bars:Market_bar.t list
  -> t Or_error.t
