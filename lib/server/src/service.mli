(** Request handling: the sexp API endpoints from {!Execlab_protocol} plus
    static file serving for the client. A client-visible failure (bad alpha,
    unknown day, wrong passcode) travels as the [Error] arm of the response
    payload with status 200; protocol-level junk gets 404/405.

    Every authenticated route resolves the request's token through
    {!Accounts} and then overwrites [config.player] with the account it
    named, so a run's owner is whoever holds the token and never whatever the
    client typed. *)

open! Core

(** [handle] routes one request.

    POST {!Execlab_protocol.Create_account.path} and
    {!Execlab_protocol.Sign_in.path} return a session token.

    POST {!Execlab_protocol.Save_run.path} grades a run under the config's
    own fill model and files it in the caller's notebook, unpublished;
    {!Execlab_protocol.My_runs.path} lists that notebook.

    POST {!Execlab_protocol.Submit_run.path} re-runs the config under
    canonical house physics, stores the server's own grading on the board,
    and records the run as published in the caller's notebook;
    {!Execlab_protocol.Leaderboard.path} lists a board.

    GET serves files under [root] ("/" means the client app), from an
    allow-list that excludes [runs_dir] — that tree holds account files.
    [data_dir] is the market data; [runs_dir] is everything {!Store}
    persists, accounts included. *)
val handle
  :  data_dir:string
  -> runs_dir:string
  -> root:string
  -> request:Http.Request.t
  -> writer:Out_channel.t
  -> unit
