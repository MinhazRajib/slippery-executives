open! Core
open! Execlab_types
open Execlab_protocol

let now_string () =
  Time_ns.to_string_utc (Time_ns.now ())
  |> String.tr ~target:' ' ~replacement:'T'
;;

let params_of_config (config : Run_config.t) =
  { Execlab_session.Params.fill_config =
      { half_spread = Price.of_int_cents config.half_spread_cents
      ; max_participation = config.max_participation
      ; impact_coefficient =
          Price.of_int_cents config.impact_coefficient_cents
      }
  ; pov_rate = config.pov_rate
  ; is_urgency = config.is_urgency
  }
;;

(* The server never trusts a client's numbers: it re-runs the submitted
   config itself (the simulator is deterministic, so the config is the proof
   of its own score) and stores only its own grading. *)
let submit_run ~data_dir ~runs_dir (config : Run_config.t) =
  let open Or_error.Let_syntax in
  let%bind () =
    if String.is_empty (String.strip config.player)
    then Or_error.error_string "player name is empty"
    else Ok ()
  in
  let%bind day =
    Catalog.load ~data_dir ~symbol:config.symbol ~date:config.date
  in
  let%bind parsed = Execlab_alpha.Parser.parse config.alpha_text in
  let%bind outcome =
    Execlab_session.run
      ~day
      ~forecast_days:
        (Catalog.forecast_days
           ~data_dir
           ~symbol:config.symbol
           ~excluding:config.date)
      ~instructions:parsed.instructions
      ~algo_name:config.algo_name
      ~params:(params_of_config config)
  in
  let summary =
    { Run_summary.value_add_cents =
        Execlab_session.Outcome.value_add_cents outcome
    ; net_cents = Execlab_session.Outcome.net_cents outcome
    ; gross_cents = Execlab_session.Outcome.gross_cents outcome
    ; alpha_capture = Execlab_session.Outcome.alpha_capture outcome
    ; shortfall_cents = Execlab_session.Outcome.shortfall_cents outcome
    }
  in
  let record =
    { Store.Record.config; summary; submitted_at = now_string () }
  in
  Store.save ~runs_dir record;
  let leaderboard =
    Store.load_board
      ~runs_dir
      ~symbol:config.symbol
      ~date:config.date
      ~alpha_hash:(alpha_hash config.alpha_text)
  in
  Ok { Submit_run.Response.summary; leaderboard }
;;

let leaderboard ~runs_dir (request : Leaderboard.Request.t) =
  Ok
    { Leaderboard.Response.rows =
        Store.load_board
          ~runs_dir
          ~symbol:request.symbol
          ~date:request.date
          ~alpha_hash:request.alpha_hash
    }
;;

(* An api endpoint: parse the sexp body, handle, answer with the sexp of
   [response Or_error.t] — a client-visible error is a payload, not a
   protocol failure, so the status is still 200. *)
let api_response
  (type req resp)
  ~writer
  ~body
  ~(req_of_sexp : Sexp.t -> req)
  ~(sexp_of_resp : resp -> Sexp.t)
  ~(handle : req -> resp Or_error.t)
  =
  let response =
    Or_error.try_with_join (fun () ->
      handle (req_of_sexp (Sexp.of_string body)))
  in
  let payload =
    match response with
    | Ok resp -> Sexp.List [ Sexp.Atom "Ok"; sexp_of_resp resp ]
    | Error error -> [%sexp Error (error : Error.t)]
  in
  Http.respond
    writer
    ~status:"200 OK"
    ~content_type:"text/plain; charset=utf-8"
    (Sexp.to_string_hum payload)
;;

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
      if String.is_empty relative || String.is_suffix relative ~suffix:"/"
      then relative ^/ "app/ui/index.html"
      else if String.equal relative "app/ui"
      then "app/ui/index.html"
      else relative
    in
    let file = root ^/ relative in
    (match Sys_unix.is_file file with
     | `Yes ->
       Http.respond
         writer
         ~status:"200 OK"
         ~content_type:(Http.content_type_of_path file)
         (In_channel.read_all file)
     | `No | `Unknown -> not_found ())
;;

let handle ~data_dir ~runs_dir ~root ~(request : Http.Request.t) ~writer =
  match request.meth, request.path with
  | "POST", path when String.equal path Submit_run.path ->
    api_response
      ~writer
      ~body:request.body
      ~req_of_sexp:[%of_sexp: Submit_run.Request.t]
      ~sexp_of_resp:[%sexp_of: Submit_run.Response.t]
      ~handle:(submit_run ~data_dir ~runs_dir)
  | "POST", path when String.equal path Leaderboard.path ->
    api_response
      ~writer
      ~body:request.body
      ~req_of_sexp:[%of_sexp: Leaderboard.Request.t]
      ~sexp_of_resp:[%sexp_of: Leaderboard.Response.t]
      ~handle:(leaderboard ~runs_dir)
  | "GET", path -> serve_static ~root ~writer path
  | (_ : string), (_ : string) ->
    Http.respond
      writer
      ~status:"405 Method Not Allowed"
      ~content_type:"text/plain"
      "method not allowed"
;;
