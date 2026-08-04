(** The execlab server: serves the client app and the sexp API.

    [dune exec bin/server.exe -- 8081] — then open http://localhost:8081/
    (the client), while POST /api/* carries the protocol. Data is read from
    [data/], submitted runs live under [runs/]. *)

open! Core
open Execlab_server

let () =
  let port =
    match Sys.get_argv () with
    | [| _ |] -> 8081
    | [| _; port |] -> Int.of_string port
    | _ ->
      eprintf "usage: server.exe [port]\n";
      exit 2
  in
  never_returns
    (Http.serve_forever
       ~port
       ~handle:(Service.handle ~data_dir:"data" ~runs_dir:"runs" ~root:"."))
;;
