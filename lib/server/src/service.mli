(** Request handling: the two sexp API endpoints from {!Execlab_protocol}
    plus static file serving for the client. A client-visible failure (bad
    alpha, unknown day) travels as the [Error] arm of the response payload
    with status 200; protocol-level junk gets 404/405. *)

open! Core

(** [handle] routes one request: POST {!Execlab_protocol.Submit_run.path}
    re-runs the submitted config and stores the server's own grading; POST
    {!Execlab_protocol.Leaderboard.path} lists a board; GET serves files
    under [root] ("/" means the client app). *)
val handle
  :  data_dir:string
  -> runs_dir:string
  -> root:string
  -> request:Http.Request.t
  -> writer:Out_channel.t
  -> unit
