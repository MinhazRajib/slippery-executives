(** What market data is on disk: the native mirror of the client's embedded
    [Dataset], driving day discovery for the CLI and the server. All
    functions swallow filesystem noise (missing directory, stray files) into
    empty results — a malformed catalog entry is not an error, it just does
    not exist. *)

open! Core
open! Execlab_types
open! Execlab_market

val symbols : data_dir:string -> Symbol.t list
val dates_for : data_dir:string -> symbol:Symbol.t -> Date.t list

val load
  :  data_dir:string
  -> symbol:Symbol.t
  -> date:Date.t
  -> Trading_day.t Or_error.t

(** Every loadable session for [symbol] except [excluding] — VWAP's
    leave-one-out forecast material (see {!Execlab_session}). *)
val forecast_days
  :  data_dir:string
  -> symbol:Symbol.t
  -> excluding:Date.t
  -> Trading_day.t list

(** The distinct symbols an alpha touches, ascending — what a run must load
    sessions for. *)
val symbols_of_instructions : Alpha_instruction.t list -> Symbol.t list

(** Every named symbol's session on [date], as one universe. Errors if any of
    them is missing, naming the one that is: a run that silently dropped a
    symbol would grade an alpha nobody wrote. *)
val universe
  :  data_dir:string
  -> date:Date.t
  -> symbols:Symbol.t list
  -> Universe.t Or_error.t

(** Leave-one-out forecast material for each symbol, ready for
    {!Execlab_session.run}. *)
val forecast_days_for
  :  data_dir:string
  -> date:Date.t
  -> symbols:Symbol.t list
  -> Trading_day.t list Symbol.Map.t
