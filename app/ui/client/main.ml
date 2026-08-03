(* ExecLab client entry point: one {!Bonsai.state'} holding the whole
   {!Model.t}, a top bar with the wizard step trail and the theme toggle, and
   a [match%sub] over the current screen. See [screens.ml] and [simulate.ml]
   for the screens themselves. *)

open! Core
open! Bonsai_web
open Bonsai.Let_syntax

let steps =
  [ Model.Screen.Choose_day, "Day"
  ; Model.Screen.Alpha, "Alpha"
  ; Model.Screen.Algo, "Algorithm"
  ; Model.Screen.Confirm, "Review"
  ; Model.Screen.Simulate, "Simulate"
  ; Model.Screen.Results, "Results"
  ]
;;

(* The step trail: earlier steps are clickable, the current one is lit, and
   Simulate/Results are reachable out of order once a run exists. *)
let topbar ~screen ~theme ~has_output ~(update : Model.updater) =
  let home = Screens.on_click (Screens.goto Model.Screen.Dashboard update) in
  let step_views =
    match Model.Screen.step_index screen with
    | None -> []
    | Some current ->
      List.concat_mapi steps ~f:(fun index (target, label) ->
        let needs_run =
          match target with
          | Model.Screen.Simulate | Model.Screen.Results -> true
          | Model.Screen.Dashboard | Model.Screen.Choose_day
          | Model.Screen.Alpha | Model.Screen.Algo | Model.Screen.Confirm ->
            false
        in
        let reachable = if needs_run then has_output else index < current in
        let state_class =
          if index = current
          then "active"
          else if reachable
          then "done"
          else ""
        in
        let classes = Vdom.Attr.classes [ "step"; state_class ] in
        let nav =
          if reachable && index <> current
          then Screens.on_click (Screens.goto target update)
          else Vdom.Attr.empty
        in
        let sep =
          if index = 0
          then Vdom.Node.none
          else {%html|<span class="step-sep">→</span>|}
        in
        [ sep
        ; {%html|<button %{classes} %{nav}>#{Int.to_string (index + 1)}. #{label}</button>|}
        ])
  in
  let theme_icon =
    match (theme : Model.Theme.t) with Dark -> "☀" | Light -> "☾"
  in
  let toggle_theme =
    Screens.on_click
      (update (fun m -> { m with Model.theme = Model.Theme.flip m.theme }))
  in
  {%html|
    <div class="topbar">
      <div class="wordmark" %{home}>Exec<span>Lab</span></div>
      <div class="steps">*{step_views}</div>
      <div class="topbar-right">
        <span class="topbar-note">historical execution laboratory</span>
        <button class="theme-btn" title="Toggle light/dark" %{toggle_theme}>#{theme_icon}</button>
      </div>
    </div>
  |}
;;

(* Start from what the last visit left in localStorage; anything unreadable
   falls back to the defaults. *)
let initial_model =
  let theme =
    match Storage.get Storage.theme_key with
    | None -> Model.initial.Model.theme
    | Some raw ->
      (match
         Or_error.try_with (fun () ->
           Model.Theme.t_of_sexp (Sexp.of_string raw))
       with
       | Ok theme -> theme
       | Error (_ : Error.t) -> Model.initial.Model.theme)
  in
  let runs =
    match Storage.get Storage.runs_key with
    | None -> []
    | Some raw ->
      (match
         Or_error.try_with (fun () ->
           [%of_sexp: Model.Run_record.t list] (Sexp.of_string raw))
       with
       | Ok runs -> runs
       | Error (_ : Error.t) -> [])
  in
  { Model.initial with Model.theme; runs }
;;

let app (local_ graph) =
  let model, update = Bonsai.state' initial_model graph in
  (* Persist the theme and the run history whenever they change. *)
  let theme =
    let%arr { Model.theme; _ } = model in
    theme
  in
  Bonsai.Edge.on_change
    ~equal:[%equal: Model.Theme.t]
    theme
    ~callback:
      (Bonsai.return (fun theme ->
         Effect.of_sync_fun
           (fun theme ->
             Storage.set
               Storage.theme_key
               (Sexp.to_string (Model.Theme.sexp_of_t theme)))
           theme))
    graph;
  let runs =
    let%arr { Model.runs; _ } = model in
    runs
  in
  Bonsai.Edge.on_change
    ~equal:[%equal: Model.Run_record.t list]
    runs
    ~callback:
      (Bonsai.return (fun runs ->
         Effect.of_sync_fun
           (fun runs ->
             Storage.set
               Storage.runs_key
               (Sexp.to_string ([%sexp_of: Model.Run_record.t list] runs)))
           runs))
    graph;
  let screen =
    let%arr { Model.screen; _ } = model in
    screen
  in
  let body =
    match%sub screen with
    | Model.Screen.Dashboard ->
      Screens.Dashboard.component ~model ~update graph
    | Model.Screen.Choose_day ->
      Screens.Choose_day.component ~model ~update graph
    | Model.Screen.Alpha ->
      Screens.Alpha_screen.component ~model ~update graph
    | Model.Screen.Algo -> Screens.Algo_screen.component ~model ~update graph
    | Model.Screen.Confirm -> Screens.Confirm.component ~model ~update graph
    | Model.Screen.Simulate -> Simulate.component ~model ~update graph
    | Model.Screen.Results -> Screens.Results.component ~model ~update graph
  in
  let%arr body
  and screen
  and update
  and { Model.output; theme; _ } = model in
  let bar =
    topbar ~screen ~theme ~has_output:(Option.is_some output) ~update
  in
  let root_classes =
    Vdom.Attr.classes
      ([ "app-root" ]
       @
       match theme with
       | Model.Theme.Light -> [ "light" ]
       | Model.Theme.Dark -> [])
  in
  {%html|<div %{root_classes}>%{bar}%{body}</div>|}
;;

let () = Bonsai_web.Start.start app
