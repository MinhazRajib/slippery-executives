open! Core
open! Execlab_types

val parse
  :  symbol:Symbol.t
  -> date:Date.t
  -> string
  -> Trading_day.t Or_error.t

val load
  :  ?data_dir:string (** default ["data"] *)
  -> symbol:Symbol.t
  -> date:Date.t
  -> unit
  -> Trading_day.t Or_error.t
