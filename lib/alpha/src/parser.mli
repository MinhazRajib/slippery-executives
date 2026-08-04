(** The alpha file: what a user's strategy decided to trade, as rows of

    {v arrival_time,symbol,side,quantity,deadline v}
    {v 09:30:00,AAPL,BUY,100,10:15:00 v}

    A parent order activates at [arrival_time] and must be finished by
    [deadline]; {!Execlab_types.Alpha_instruction.create} enforces the rest
    (positive quantity, ordered times, market hours).

    These files come from other people's tooling, so parsing is forgiving
    about shape and strict about content: an optional header row is skipped,
    blank lines are ignored, and whitespace around the commas is trimmed.
    Every bad row is reported with its line number rather than only the
    first, so a user fixes one file once. *)

open! Core
open! Execlab_types

type t = { instructions : Alpha_instruction.t list }
[@@deriving sexp, bin_io, compare, equal]

(** Errors carry every offending line number, not just the earliest. An empty
    file parses to no instructions — that is a legitimate, if idle, alpha. *)
val parse : string -> t Or_error.t
