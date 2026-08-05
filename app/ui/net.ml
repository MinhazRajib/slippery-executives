(* The client half of the protocol seam: sexp requests over XmlHttpRequest,
   answers as [resp Or_error.t] deferreds. A server that is down or
   unreachable becomes an [Error], never an exception — the app must stay
   fully usable offline. *)

open! Core
open Async_kernel
open Js_of_ocaml
open Execlab_protocol

let post ~path ~body =
  Deferred.create (fun result ->
    let xhr = XmlHttpRequest.create () in
    xhr##_open (Js.string "POST") (Js.string path) Js._true;
    xhr##.onreadystatechange
    := Js.wrap_callback (fun () ->
         match xhr##.readyState with
         | XmlHttpRequest.DONE ->
           Ivar.fill_if_empty
             result
             (if xhr##.status = 200
              then
                Ok
                  (Js.Opt.case
                     xhr##.responseText
                     (fun () -> "")
                     Js.to_string)
              else
                Or_error.error_s
                  [%message
                    "server unreachable" ~status:(xhr##.status : int)])
         | UNSENT | OPENED | HEADERS_RECEIVED | LOADING -> ());
    xhr##send (Js.some (Js.string body)))
;;

let parse_response ~resp_of_sexp text =
  Or_error.try_with_join (fun () ->
    match (Sexp.of_string (String.strip text) : Sexp.t) with
    | List [ Atom "Ok"; payload ] -> Ok (resp_of_sexp payload)
    | List [ Atom "Error"; payload ] -> Error (Error.t_of_sexp payload)
    | sexp ->
      Or_error.error_s [%message "unparseable response" (sexp : Sexp.t)])
;;

let call ~path ~sexp_of_req ~resp_of_sexp request =
  match%map post ~path ~body:(Sexp.to_string (sexp_of_req request)) with
  | Error (_ : Error.t) as error -> error
  | Ok text -> parse_response ~resp_of_sexp text
;;

let create_account credentials =
  call
    ~path:Create_account.path
    ~sexp_of_req:[%sexp_of: Create_account.Request.t]
    ~resp_of_sexp:[%of_sexp: Create_account.Response.t]
    credentials
;;

let sign_in credentials =
  call
    ~path:Sign_in.path
    ~sexp_of_req:[%sexp_of: Sign_in.Request.t]
    ~resp_of_sexp:[%of_sexp: Sign_in.Response.t]
    credentials
;;

let save_run request =
  call
    ~path:Save_run.path
    ~sexp_of_req:[%sexp_of: Save_run.Request.t]
    ~resp_of_sexp:[%of_sexp: Save_run.Response.t]
    request
;;

let my_runs request =
  call
    ~path:My_runs.path
    ~sexp_of_req:[%sexp_of: My_runs.Request.t]
    ~resp_of_sexp:[%of_sexp: My_runs.Response.t]
    request
;;

let submit_run request =
  call
    ~path:Submit_run.path
    ~sexp_of_req:[%sexp_of: Submit_run.Request.t]
    ~resp_of_sexp:[%of_sexp: Submit_run.Response.t]
    request
;;

let reset_account request =
  call
    ~path:Reset_account.path
    ~sexp_of_req:[%sexp_of: Reset_account.Request.t]
    ~resp_of_sexp:[%of_sexp: Reset_account.Response.t]
    request
;;

let leaderboard request =
  call
    ~path:Leaderboard.path
    ~sexp_of_req:[%sexp_of: Leaderboard.Request.t]
    ~resp_of_sexp:[%of_sexp: Leaderboard.Response.t]
    request
;;
