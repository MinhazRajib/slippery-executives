(** Request handling: static file serving for the client app. GET serves
    files under [root] ("/" means the client); anything else is 404/405. The
    lab is local and single-user, so there is no API. *)

open! Core

val handle
  :  root:string
  -> request:Http.Request.t
  -> writer:Out_channel.t
  -> unit
