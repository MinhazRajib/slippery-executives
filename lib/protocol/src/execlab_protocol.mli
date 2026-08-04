(** The one client<->server seam: request/response types for the thin HTTP
    API, shared verbatim by the browser client (via js_of_ocaml) and the
    native server. Bodies travel as sexps of these types; a response body is
    the sexp of [Response.t Or_error.t].

    Versioning is by rebuild — both sides always compile from the same tree —
    so there are no stable witnesses here yet; they arrive if the protocol
    ever has to span releases. *)

open! Core
open! Execlab_types

module Run_config : sig
  (** Everything needed to reproduce a run bit-for-bit. The server
      re-executes this config itself rather than trusting client-side results
      — the simulator is deterministic, so a submitted config {e is} the
      proof of its own score. *)
  type t =
    { player : string
    ; symbol : Symbol.t
    ; date : Date.t
    ; alpha_text : string
    ; algo_name : string
    (** ["twap" | "vwap" | "pov" | "is" | "immediate"] *)
    ; half_spread_cents : int
    ; max_participation : float
    ; impact_coefficient_cents : int
    ; pov_rate : float
    ; is_urgency : float
    ; engine_name : string (** ["bar" | "synthetic"] *)
    ; engine_seed : int (** meaningful for ["synthetic"] only *)
    }
  [@@deriving sexp, equal]
end

module Run_summary : sig
  (** The aggregate grade of one run, totalled across its orders. *)
  type t =
    { value_add_cents : int (** vs the immediate baseline *)
    ; net_cents : int
    ; gross_cents : int
    ; alpha_capture : float option
    ; shortfall_cents : int
    }
  [@@deriving sexp, equal]
end

module Leaderboard_row : sig
  type t =
    { player : string
    ; algo_name : string
    ; submitted_at : string (** ISO date-time, server clock *)
    ; summary : Run_summary.t
    }
  [@@deriving sexp, equal]
end

module Submit_run : sig
  val path : string

  module Request = Run_config

  module Response : sig
    (** The server's own grading of the config, plus the refreshed same-day
        same-alpha leaderboard it now sits in. *)
    type t =
      { summary : Run_summary.t
      ; leaderboard : Leaderboard_row.t list
      }
    [@@deriving sexp, equal]
  end
end

module Leaderboard : sig
  val path : string

  module Request : sig
    type t =
      { symbol : Symbol.t
      ; date : Date.t
      ; alpha_hash : string (** see {!alpha_hash} *)
      }
    [@@deriving sexp, equal]
  end

  module Response : sig
    type t = { rows : Leaderboard_row.t list } [@@deriving sexp, equal]
  end
end

(** The identity of an alpha for leaderboard grouping: md5 of the text with
    blank lines and edge whitespace stripped, so cosmetic edits don't fork
    the board. *)
val alpha_hash : string -> string
