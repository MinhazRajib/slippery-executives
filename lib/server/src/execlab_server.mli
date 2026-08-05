(** The execlab server: a tiny static file server for the client app.
    {!Catalog} discovers market data on disk for the CLI, {!Http} carries
    bytes, {!Service} routes GETs. [bin/server.exe] is the entry point. *)

module Catalog = Catalog
module Http = Http
module Service = Service
