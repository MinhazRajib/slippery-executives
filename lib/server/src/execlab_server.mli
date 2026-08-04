(** The execlab server: a tiny blocking HTTP front over the shared session
    pipeline. {!Catalog} discovers market data on disk, {!Accounts} owns
    usernames and session tokens, {!Store} persists boards and each account's
    own runs as sexp files, {!Http} carries bytes, {!Service} routes.
    [bin/server.exe] is the entry point. *)

module Accounts = Accounts
module Catalog = Catalog
module Http = Http
module Service = Service
module Store = Store
