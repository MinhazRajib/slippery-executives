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
