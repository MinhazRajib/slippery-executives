(** Run persistence — sexp files in a directory tree, no database:

    {v
      <runs_dir>/boards/<SYMBOL>-<date>-<alpha-hash>-<engine>/<file>.sexp
      <runs_dir>/users/<username>/<run_id>.sexp
      <runs_dir>/accounts/<username>.sexp        (see {!Accounts})
    v}

    A leaderboard {e is} a directory listing, and an account's notebook is
    another one. Corrupt or foreign files are skipped, never fatal: a board
    that half-parses still ranks. Board ranking is by value-add over the
    immediate baseline, best first.

    Board files keep the whole {!Record.t}, config included, so the server
    can re-grade or audit a submission — but {!Record.row} publishes only
    who, when, and the score. The strategy stays with its owner, who sees it
    on their own {!Execlab_protocol.Saved_run.t}. *)

open! Core
open! Execlab_types
open Execlab_protocol

module Record : sig
  (** One published submission, as stored under [boards/]. *)
  type t =
    { config : Run_config.t
    ; summary : Run_summary.t
    ; submitted_at : string
    }
  [@@deriving sexp]

  (** The public projection: {!Execlab_protocol.Leaderboard_row.t} carries no
      algorithm and no parameters, so reading the board teaches you nothing
      about how a rival traded. *)
  val row : t -> Leaderboard_row.t
end

(** Where {!Accounts} keeps its files. Every path under [runs_dir] is this
    module's business, including the one it does not read itself. *)
val accounts_dir : runs_dir:string -> string

(** A short, stable, filename-safe identity for one run: hex digest of the
    config and the timestamp it was graded at. The same config graded twice
    gets two ids — a notebook is a log, not a set. *)
val run_id : config:Run_config.t -> ran_at:string -> string

(** Appends to the board named by the record's own config. *)
val save : runs_dir:string -> Record.t -> unit

val load_board
  :  runs_dir:string
  -> symbol:Symbol.t
  -> date:Date.t
  -> alpha_hash:string
  -> engine_name:string
  -> Leaderboard_row.t list

(** Writes (or overwrites) one entry of [username]'s notebook, keyed by
    {!Execlab_protocol.Saved_run.run_id}. *)
val save_user_run : runs_dir:string -> username:string -> Saved_run.t -> unit

(** [username]'s notebook, newest {!Execlab_protocol.Saved_run.ran_at} first.
    An account that has never run anything has an empty one. *)
val load_user_runs : runs_dir:string -> username:string -> Saved_run.t list

val find_user_run
  :  runs_dir:string
  -> username:string
  -> run_id:string
  -> Saved_run.t option

(** Flips an existing entry's [published] flag on, for when a saved run is
    later submitted to its board. A run that is not in the notebook (or is
    unreadable) is left alone — publishing must not fail over bookkeeping. *)
val mark_published
  :  runs_dir:string
  -> username:string
  -> run_id:string
  -> unit
