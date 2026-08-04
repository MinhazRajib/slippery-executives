(** Run persistence: one sexp file per submitted run, grouped into a
    directory per (symbol, date, alpha-hash) — a leaderboard {e is} a
    directory listing. Corrupt or foreign files are skipped, never fatal;
    ranking is by value-add over the immediate baseline, best first. *)

open! Core
open! Execlab_types
open Execlab_protocol

module Record : sig
  type t =
    { config : Run_config.t
    ; summary : Run_summary.t
    ; submitted_at : string
    }
  [@@deriving sexp]

  val row : t -> Leaderboard_row.t
end

val save : runs_dir:string -> Record.t -> unit

val load_board
  :  runs_dir:string
  -> symbol:Symbol.t
  -> date:Date.t
  -> alpha_hash:string
  -> engine_name:string
  -> Leaderboard_row.t list
