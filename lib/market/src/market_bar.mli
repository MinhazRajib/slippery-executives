(** A market bar represents the open, high, low, and close prices for a given
    time period.

    Example: 09:35:00,388.805,389.18,388.0149,388.27,167652

    The create validates that the high is greater than or equal to the low,
    and that the open and close are between the high and low. It also
    validates that the volume is non-negative. *)

open! Core
open! Execlab_types

type t = private
  { time : Time_ns.Ofday.t
  ; open_ : Price.t
  ; high : Price.t
  ; low : Price.t
  ; close : Price.t
  ; volume : Size.t
  }
[@@deriving sexp, bin_io, compare, equal]

val create
  :  time:Time_ns.Ofday.t
  -> open_:Price.t
  -> high:Price.t
  -> low:Price.t
  -> close:Price.t
  -> volume:Size.t
  -> t Or_error.t
