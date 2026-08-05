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
      proof of its own score.

      [player] is advisory: on any authenticated call the server overwrites
      it with the token's account, so a config cannot claim someone else's
      name. *)
  type t =
    { player : string
    ; symbols : Symbol.t list
    (** every name [alpha_text] trades, ascending; derived from it, and
        carried separately only so a board can be found without reparsing *)
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
  (** The aggregate grade of one run, totalled across its orders. Cents are
      [Int63.t], not [int]: the browser client is js_of_ocaml, where [int] is
      32 bits, and a large enough position (a few million shares of a $200
      name) exceeds that — as an [int] those totals arrive as an unparseable
      sexp and poison every later read of the board. *)
  type t =
    { value_add_cents : Int63.t (** vs the immediate baseline *)
    ; net_cents : Int63.t
    ; gross_cents : Int63.t
    ; alpha_capture : float option
    ; shortfall_cents : Int63.t
    }
  [@@deriving sexp, equal]

  val of_cents
    :  value_add:int
    -> net:int
    -> gross:int
    -> alpha_capture:float option
    -> shortfall:int
    -> t

  (** Dollars, for display only. *)
  val dollars : Int63.t -> float
end

(** {2 Accounts}

    Deliberately minimal: a username and a passcode, no email, no recovery,
    no OAuth. The whole point is that a run can carry an owner across devices
    and sessions; it is not a security boundary, and the [token] is a bearer
    credential derived from the stored passcode digest — treat it as a
    passcode equivalent. Guests never obtain one and can still use every part
    of the app except publishing. *)

module Credentials : sig
  type t =
    { username : string
    ; passcode : string
    }
  [@@deriving sexp, equal]
end

module Session : sig
  (** What the client persists to stay signed in. *)
  type t =
    { username : string
    ; token : string
    }
  [@@deriving sexp, equal]
end

(** Errors if the username is taken or either field fails validation. *)
module Create_account : sig
  val path : string

  module Request = Credentials
  module Response = Session
end

(** Errors if the account is unknown or the passcode does not match. *)
module Sign_in : sig
  val path : string

  module Request = Credentials
  module Response = Session
end

(** {2 Runs} *)

module Leaderboard_row : sig
  (** A published run as {e other people} see it: who, when, and how it
      scored — deliberately {b not} which algorithm or parameters produced
      it. The board is a scoreboard, not a strategy leak; the owner sees the
      full config on their own {!Saved_run}. *)
  type t =
    { player : string
    ; submitted_at : string (** ISO date-time, server clock *)
    ; summary : Run_summary.t
    }
  [@@deriving sexp, equal]
end

module Saved_run : sig
  (** One entry in an account's own notebook: the complete config (so the
      results screen can be reopened by replaying it) plus the server's
      grading. [published] is true once the run has been submitted to its
      board. *)
  type t =
    { run_id : string
    ; config : Run_config.t
    ; summary : Run_summary.t
    ; ran_at : string (** ISO date-time, server clock *)
    ; published : bool
    }
  [@@deriving sexp, equal]
end

(** Records a run in the caller's notebook without publishing it. The server
    grades it under the config's {e own} parameters — this is the user's
    experiment, so their knobs are the point. *)
module Save_run : sig
  val path : string

  module Request : sig
    type t =
      { token : string
      ; config : Run_config.t
      }
    [@@deriving sexp, equal]
  end

  module Response : sig
    type t = { run : Saved_run.t } [@@deriving sexp, equal]
  end
end

(** The caller's own runs, newest first. *)
module My_runs : sig
  val path : string

  module Request : sig
    type t = { token : string } [@@deriving sexp, equal]
  end

  module Response : sig
    type t = { runs : Saved_run.t list } [@@deriving sexp, equal]
  end
end

(** Publishes a run to its board. Unlike {!Save_run}, the server grades it
    under canonical house physics, so every row on a board faced the
    identical market. Requires an account: guests must sign in first. *)
module Submit_run : sig
  val path : string

  module Request : sig
    type t =
      { token : string
      ; config : Run_config.t
      }
    [@@deriving sexp, equal]
  end

  module Response : sig
    (** The server's own grading of the config, plus the refreshed board it
        now sits in. *)
    type t =
      { summary : Run_summary.t
      ; leaderboard : Leaderboard_row.t list
      }
    [@@deriving sexp, equal]
  end
end

(** Erases the caller's notebook: every saved run under their account is
    deleted and the count returned. Published board rows are {e not}
    withdrawn — a leaderboard entry is part of the shared record other
    players competed against. The account itself survives. *)
module Reset_account : sig
  val path : string

  module Request : sig
    type t = { token : string } [@@deriving sexp, equal]
  end

  module Response : sig
    type t = { deleted_runs : int } [@@deriving sexp, equal]
  end
end

module Leaderboard : sig
  val path : string

  module Request : sig
    type t =
      { symbols : Symbol.t list
      ; date : Date.t
      ; alpha_hash : string (** see {!alpha_hash} *)
      ; engine_name : string (** boards are per-engine *)
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
