(** Request handling for the local lab: static file serving only. The app is
    fully client-side — simulations run in the browser and history lives in
    its localStorage — so the server's whole job is delivering the client and
    its public inputs. *)

open! Core

let serve_static ~root ~writer path =
  let not_found () =
    Http.respond
      writer
      ~status:"404 Not Found"
      ~content_type:"text/plain"
      "not found"
  in
  match Http.safe_relative_path path with
  | None -> not_found ()
  | Some relative ->
    let relative =
      if String.is_empty relative
         || String.is_suffix relative ~suffix:"/"
         || String.equal relative "app/ui"
      then "app/ui/index.html"
      else relative
    in
    (* The server's root is the repo checkout, which also holds source and
       git history — only the client app and its public inputs are servable. *)
    let allowed =
      List.exists
        [ "app/"; "_build/"; "data/"; "examples/" ]
        ~f:(fun prefix -> String.is_prefix relative ~prefix)
    in
    if not allowed
    then not_found ()
    else (
      let file = root ^/ relative in
      match Sys_unix.is_file file with
      | `Yes ->
        Http.respond
          writer
          ~status:"200 OK"
          ~content_type:(Http.content_type_of_path file)
          (In_channel.read_all file)
      | `No | `Unknown -> not_found ())
;;

let handle ~root ~(request : Http.Request.t) ~writer =
  match request.meth, request.path with
  | "GET", path -> serve_static ~root ~writer path
  | (_ : string), (_ : string) ->
    Http.respond
      writer
      ~status:"405 Method Not Allowed"
      ~content_type:"text/plain"
      "method not allowed"
;;
