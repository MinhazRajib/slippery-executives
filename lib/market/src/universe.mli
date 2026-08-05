(** The set of sessions one run trades across: several symbols on the same
    date, stepped by a single clock.

    Every {!Trading_day} is exactly 390 strictly ascending minutes from 09:30
    to 15:59, so sessions of the same date are aligned by construction —
    minute [m] means the same wall-clock instant in every symbol, and a run
    can advance them in lockstep without interpolating or aligning anything.

    A single-symbol run is just a universe of one ({!of_day}), so the driver
    has one code path rather than two. *)

open! Core
open! Execlab_types

type t

(** Errors unless the sessions share a date and no symbol appears twice.
    Empty is an error: a run with no market is not a run. *)
val of_days : Trading_day.t list -> t Or_error.t

(** The single-symbol universe. Cannot fail: one session is always a
    consistent set. *)
val of_day : Trading_day.t -> t

val date : t -> Date.t

(** Ascending, so orderings derived from it are stable. *)
val symbols : t -> Symbol.t list

val days : t -> Trading_day.t list
val mem : t -> Symbol.t -> bool
val day : t -> Symbol.t -> Trading_day.t option

(** Raises for a symbol outside the universe: callers reaching for a session
    they never loaded is a programming error, not user input. Validate an
    alpha's symbols against {!mem} before running it. *)
val day_exn : t -> Symbol.t -> Trading_day.t

(** 390 for a normal session. *)
val minutes : t -> int

val time_at : t -> minute:int -> Time_ns.Ofday.t

(** [minute] is an index into the session, not a time of day. Raises like
    {!day_exn}. *)
val bar_exn : t -> symbol:Symbol.t -> minute:int -> Market_bar.t
