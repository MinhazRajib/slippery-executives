open! Core
open! Execlab_types
open Execlab_protocol

let now_string () =
  Time_ns.to_string_utc (Time_ns.now ())
  |> String.tr ~target:' ' ~replacement:'T'
;;

let finite name value ~check ~message =
  if Float.is_finite value && check value
  then Ok value
  else
    Or_error.error_s
      [%message "bad parameter" ~_:(name : string) (value : float) message]
;;

(* The algorithm's own knobs and the choice of market: shared by both
   gradings, because a config that names an impossible participation rate is
   junk whether or not it is going on a board. *)
let params_of_config ~fill_config (config : Run_config.t) =
  let open Or_error.Let_syntax in
  let%bind pov_rate =
    finite
      "pov_rate"
      config.pov_rate
      ~check:(fun v -> Float.( > ) v 0. && Float.( <= ) v 1.)
      ~message:"a fraction in (0, 1]"
  in
  let%map is_urgency =
    finite
      "is_urgency"
      config.is_urgency
      ~check:(fun v -> Float.( >= ) v 0. && Float.( <= ) v 10_000.)
      ~message:"in [0, 10000]"
  in
  { Execlab_session.Params.fill_config
  ; pov_rate
  ; is_urgency
  ; engine =
      (match config.engine_name with
       | "synthetic" ->
         Execlab_session.Engine_choice.Synthetic
           { seed =
               String.fold
                 (alpha_hash config.alpha_text)
                 ~init:17
                 ~f:(fun acc c -> ((acc * 31) + Char.to_int c) % 100_003)
           }
       | (_ : string) -> Execlab_session.Engine_choice.Bar_model)
  }
;;

(* A leaderboard compares {e executions}, so the physics must be the house's,
   not the submitter's: the fill model is pinned to its defaults and a
   synthetic run's seed is derived from the board's own identity, so everyone
   on a board faces the identical market and nobody wins by turning impact
   off or shopping for a kind seed. The algorithm's own knobs stay free —
   those are the strategy. *)
let canonical_params (config : Run_config.t) =
  params_of_config
    ~fill_config:Execlab_simulation.Fill_model.Config.default
    config
;;

(* A private run is the opposite case: the user's fill model {e is} the
   experiment ("what if the spread were 5c?"), so their numbers stand — but
   they are still human input, so they are still checked. *)
let own_params (config : Run_config.t) =
  let open Or_error.Let_syntax in
  let cents name value =
    match value >= 0 with
    | true -> Ok (Price.of_int_cents value)
    | false ->
      Or_error.error_s
        [%message
          "bad parameter" ~_:(name : string) (value : int) "must be >= 0"]
  in
  let%bind half_spread =
    cents "half_spread_cents" config.half_spread_cents
  in
  let%bind impact_coefficient =
    cents "impact_coefficient_cents" config.impact_coefficient_cents
  in
  let%bind max_participation =
    finite
      "max_participation"
      config.max_participation
      ~check:(fun v -> Float.( > ) v 0. && Float.( <= ) v 1.)
      ~message:"a fraction in (0, 1]"
  in
  params_of_config
    ~fill_config:
      { Execlab_simulation.Fill_model.Config.half_spread
      ; max_participation
      ; impact_coefficient
      }
    config
;;

(* The server never trusts a client's numbers: it re-runs the submitted
   config itself (the simulator is deterministic, so the config is the proof
   of its own score) and stores only its own grading. *)
let grade ~data_dir ~(config : Run_config.t) ~params =
  let open Or_error.Let_syntax in
  let%bind parsed = Execlab_alpha.Parser.parse config.alpha_text in
  let symbols = Catalog.symbols_of_instructions parsed.instructions in
  let%bind universe =
    Catalog.universe ~data_dir ~date:config.date ~symbols
  in
  let%map outcome =
    Execlab_session.run
      ~universe
      ~forecast_days:
        (Catalog.forecast_days_for ~data_dir ~date:config.date ~symbols)
      ~instructions:parsed.instructions
      ~algo_name:config.algo_name
      ~params
  in
  Run_summary.of_cents
    ~value_add:(Execlab_session.Outcome.value_add_cents outcome)
    ~net:(Execlab_session.Outcome.net_cents outcome)
    ~gross:(Execlab_session.Outcome.gross_cents outcome)
    ~alpha_capture:(Execlab_session.Outcome.alpha_capture outcome)
    ~shortfall:(Execlab_session.Outcome.shortfall_cents outcome)
;;

let username_of_token ~runs_dir ~token =
  Accounts.username_of_token
    ~accounts_dir:(Store.accounts_dir ~runs_dir)
    ~token
;;

(* [config.player] is advisory on the wire and overwritten here: whoever
   holds the token owns the run, whatever name the client typed into the
   config. *)
let owned_config ~username (config : Run_config.t) =
  { config with player = username }
;;

let create_account ~runs_dir (request : Create_account.Request.t) =
  Accounts.create
    ~accounts_dir:(Store.accounts_dir ~runs_dir)
    ~username:request.username
    ~passcode:request.passcode
;;

let sign_in ~runs_dir (request : Sign_in.Request.t) =
  Accounts.sign_in
    ~accounts_dir:(Store.accounts_dir ~runs_dir)
    ~username:request.username
    ~passcode:request.passcode
;;

let submit_run ~data_dir ~runs_dir (request : Submit_run.Request.t) =
  let open Or_error.Let_syntax in
  let%bind username = username_of_token ~runs_dir ~token:request.token in
  let config = owned_config ~username request.config in
  let%bind params = canonical_params config in
  let%map summary = grade ~data_dir ~config ~params in
  let submitted_at = now_string () in
  let physics = Execlab_session.physics_fingerprint params in
  Store.save
    ~runs_dir
    ~physics
    { Store.Record.config; summary; submitted_at };
  (* The board is public and the notebook is the owner's copy of the same
     run, so a published run shows as published in My Runs. *)
  let run_id = Store.run_id ~config ~ran_at:submitted_at in
  (match Store.find_user_run ~runs_dir ~username ~run_id with
   | Some (_ : Saved_run.t) ->
     Store.mark_published ~runs_dir ~username ~run_id
   | None ->
     Store.save_user_run
       ~runs_dir
       ~username
       { Saved_run.run_id
       ; config
       ; summary
       ; ran_at = submitted_at
       ; published = true
       });
  let leaderboard =
    Store.load_board
      ~runs_dir
      ~symbols:config.symbols
      ~date:config.date
      ~alpha_hash:(alpha_hash config.alpha_text)
      ~engine_name:config.engine_name
      ~physics
  in
  { Submit_run.Response.summary; leaderboard }
;;

let save_run ~data_dir ~runs_dir (request : Save_run.Request.t) =
  let open Or_error.Let_syntax in
  let%bind username = username_of_token ~runs_dir ~token:request.token in
  let config = owned_config ~username request.config in
  let%bind params = own_params config in
  let%map summary = grade ~data_dir ~config ~params in
  let ran_at = now_string () in
  let run =
    { Saved_run.run_id = Store.run_id ~config ~ran_at
    ; config
    ; summary
    ; ran_at
    ; published = false
    }
  in
  Store.save_user_run ~runs_dir ~username run;
  { Save_run.Response.run }
;;

let my_runs ~runs_dir (request : My_runs.Request.t) =
  let open Or_error.Let_syntax in
  let%map username = username_of_token ~runs_dir ~token:request.token in
  { My_runs.Response.runs = Store.load_user_runs ~runs_dir ~username }
;;

let reset_account ~runs_dir (request : Reset_account.Request.t) =
  let open Or_error.Let_syntax in
  let%map username = username_of_token ~runs_dir ~token:request.token in
  { Reset_account.Response.deleted_runs =
      Store.delete_user_runs ~runs_dir ~username
  }
;;

let leaderboard ~runs_dir (request : Leaderboard.Request.t) =
  Ok
    { Leaderboard.Response.rows =
        Store.load_board
          ~runs_dir
          ~symbols:request.symbols
          ~date:request.date
          ~alpha_hash:request.alpha_hash
          ~engine_name:request.engine_name
            (* A board is asked for by its engine; the physics that engine is
               calibrated with today decide which board that is. *)
          ~physics:
            (Execlab_session.physics_fingerprint
               { Execlab_session.Params.default with
                 engine =
                   (match request.engine_name with
                    | "synthetic" ->
                      Execlab_session.Engine_choice.Synthetic { seed = 0 }
                    | (_ : string) -> Execlab_session.Engine_choice.Bar_model)
               })
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

let serve_static ~root ~runs_dir ~writer path =
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
    (* The server's root is the repo checkout, which also holds source, git
       history, and everyone's runs — only the client app and its public
       inputs are servable. [runs_dir] is emphatically not on this list: it
       holds the account files, whose passcode digests {e are} the tokens.
       Its name is also barred anywhere in the path, because [_build/] is
       servable and a future rule that copied the runs tree into it would
       otherwise publish every account. *)
    let allowed =
      List.exists
        [ "app/"; "_build/"; "data/"; "examples/" ]
        ~f:(fun prefix -> String.is_prefix relative ~prefix)
      && not
           (List.mem
              (String.split relative ~on:'/')
              (Filename.basename runs_dir)
              ~equal:String.equal)
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

let handle ~data_dir ~runs_dir ~root ~(request : Http.Request.t) ~writer =
  match request.meth, request.path with
  | "POST", path when String.equal path Create_account.path ->
    api_response
      ~writer
      ~body:request.body
      ~req_of_sexp:[%of_sexp: Create_account.Request.t]
      ~sexp_of_resp:[%sexp_of: Create_account.Response.t]
      ~handle:(create_account ~runs_dir)
  | "POST", path when String.equal path Sign_in.path ->
    api_response
      ~writer
      ~body:request.body
      ~req_of_sexp:[%of_sexp: Sign_in.Request.t]
      ~sexp_of_resp:[%sexp_of: Sign_in.Response.t]
      ~handle:(sign_in ~runs_dir)
  | "POST", path when String.equal path Save_run.path ->
    api_response
      ~writer
      ~body:request.body
      ~req_of_sexp:[%of_sexp: Save_run.Request.t]
      ~sexp_of_resp:[%sexp_of: Save_run.Response.t]
      ~handle:(save_run ~data_dir ~runs_dir)
  | "POST", path when String.equal path My_runs.path ->
    api_response
      ~writer
      ~body:request.body
      ~req_of_sexp:[%of_sexp: My_runs.Request.t]
      ~sexp_of_resp:[%sexp_of: My_runs.Response.t]
      ~handle:(my_runs ~runs_dir)
  | "POST", path when String.equal path Reset_account.path ->
    api_response
      ~writer
      ~body:request.body
      ~req_of_sexp:[%of_sexp: Reset_account.Request.t]
      ~sexp_of_resp:[%sexp_of: Reset_account.Response.t]
      ~handle:(reset_account ~runs_dir)
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
  | "GET", path -> serve_static ~root ~runs_dir ~writer path
  | (_ : string), (_ : string) ->
    Http.respond
      writer
      ~status:"405 Method Not Allowed"
      ~content_type:"text/plain"
      "method not allowed"
;;
