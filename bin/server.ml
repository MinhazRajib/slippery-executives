(** The execlab server: serves the client app over HTTP.

    [dune exec bin/server.exe -- 8081] — then open http://localhost:8081/.
    The lab is local and single-user: simulations run in the browser and
    history lives in its localStorage, so this binary only delivers files. *)

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
  never_returns (Http.serve_forever ~port ~handle:(Service.handle ~root:"."))
;;
