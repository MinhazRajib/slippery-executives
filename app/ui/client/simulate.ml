(* The live simulator screen: replays a finished run bar-by-bar with a
   play/pause control, a speed selector, and a scrubber. The clock only ticks
   while this screen is mounted; every panel — chart, parent orders, event
   log, blotter — is a pure prefix view over the batch result via {!Replay},
   so scrubbing in either direction is instant and exact. *)

open! Core
open! Execlab_types
open! Execlab_market
open! Execlab_execution
open! Bonsai_web
open Bonsai.Let_syntax

let on_click effect =
  Vdom.Attr.on_click (fun (_ : _ Js_of_ocaml.Js.t) -> effect)
;;

let speeds = [ 1; 2; 4; 10 ]
let last_minute = Replay.last_minute

let bar_time (day : Trading_day.t) minute =
  match List.nth day.bars minute with
  | Some bar -> Fmt.ofday bar.Market_bar.time
  | None -> "--:--"
;;

let toolbar
  (day : Trading_day.t)
  ~minute
  ~playing
  ~speed
  ~(update : Model.updater)
  =
  let at_end = minute >= last_minute in
  let toggle =
    on_click
      (update (fun m ->
         if at_end && not playing
         then { m with Model.sim_minute = 0; sim_playing = true }
         else { m with Model.sim_playing = not m.Model.sim_playing }))
  in
  let play_icon = if playing then "❚❚" else if at_end then "↻" else "▶" in
  let speed_buttons =
    List.map speeds ~f:(fun this_speed ->
      let classes =
        Vdom.Attr.classes
          ([ "seg-btn" ] @ if speed = this_speed then [ "selected" ] else [])
      in
      let pick =
        on_click (update (fun m -> { m with Model.sim_speed = this_speed }))
      in
      {%html|<button %{classes} %{pick}>#{Int.to_string this_speed}×</button>|})
  in
  let scrub_attrs =
    [ Vdom.Attr.classes [ "scrub" ]
    ; Vdom.Attr.create "type" "range"
    ; Vdom.Attr.create "min" "0"
    ; Vdom.Attr.create "max" (Int.to_string last_minute)
    ; Vdom.Attr.string_property "value" (Int.to_string minute)
    ; Vdom.Attr.on_input (fun (_ : _ Js_of_ocaml.Js.t) text ->
        update (fun m ->
          match Int.of_string_opt text with
          | None -> m
          | Some minute ->
            { m with
              Model.sim_minute = Int.clamp_exn minute ~min:0 ~max:last_minute
            }))
    ]
  in
  let scrubber = Vdom.Node.input ~attrs:scrub_attrs () in
  let pct = 100 * minute / last_minute in
  {%html|
    <div class="sim-toolbar">
      <button class="play-btn" %{toggle}>#{play_icon}</button>
      <div class="seg compact">*{speed_buttons}</div>
      %{scrubber}
      <span class="sim-pct">#{Int.to_string pct}% of day</span>
      <span class="sim-clock">#{bar_time day minute}</span>
    </div>
  |}
;;

let parent_panel (views : Replay.Parent_view.t list) =
  let rows =
    List.mapi views ~f:(fun index (view : Replay.Parent_view.t) ->
      let instruction = view.instruction in
      let pill_classes =
        Vdom.Attr.classes
          [ "status-pill"; Replay.Parent_view.Status.css_class view.status ]
      in
      let pct =
        if view.quantity = 0
        then 0.
        else Float.of_int view.filled /. Float.of_int view.quantity
      in
      let fill_style =
        Vdom.Attr.create "style" (sprintf "width: %.1f%%" (pct *. 100.))
      in
      let window =
        [%string
          "%{Fmt.ofday instruction.arrival_time}→%{Fmt.ofday \
           instruction.deadline}"]
      in
      let average =
        match view.average_price with
        | None -> "avg —"
        | Some price -> [%string "avg %{Fmt.dollars_4dp price}"]
      in
      let side_badge =
        match instruction.side with
        | Side.Buy -> {%html|<span class="badge buy">BUY</span>|}
        | Side.Sell -> {%html|<span class="badge sell">SELL</span>|}
      in
      let ordinal = "#" ^ Int.to_string (index + 1) in
      {%html|
        <div class="parent-row">
          <div class="parent-head">
            <span class="dim">#{ordinal}</span>
            %{side_badge}
            <span class="mono">#{Fmt.shares_int view.quantity}</span>
            <span class="mono">#{window}</span>
            <span %{pill_classes}>#{Replay.Parent_view.Status.label view.status}</span>
          </div>
          <div class="pbar"><div class="pbar-fill" %{fill_style}></div></div>
          <div class="parent-stats">
            <span>#{Fmt.shares_int view.filled} / #{Fmt.shares_int view.quantity} filled</span>
            <span>#{average}</span>
          </div>
        </div>
      |})
  in
  {%html|
    <div class="panel">
      <div class="section-label">Parent orders</div>
      *{rows}
    </div>
  |}
;;

let event_log (day : Trading_day.t) (events : Replay.Event.t list) ~minute =
  let visible =
    List.filter events ~f:(fun event -> event.Replay.Event.minute <= minute)
    |> List.rev
  in
  let rows =
    List.map visible ~f:(fun (event : Replay.Event.t) ->
      let classes =
        Vdom.Attr.classes
          [ "ev-row"; Replay.Event.Kind.css_class event.kind ]
      in
      {%html|
        <div %{classes}>
          <span class="ev-time">#{bar_time day event.minute}</span>
          <span class="ev-msg">#{event.message}</span>
        </div>
      |})
  in
  let body =
    match rows with
    | [] ->
      {%html|<div class="muted small">Waiting for the first instruction to activate…</div>|}
    | _ :: _ -> {%html|<div class="event-log">*{rows}</div>|}
  in
  {%html|
    <div class="panel">
      <div class="section-label">Event log</div>
      %{body}
    </div>
  |}
;;

let blotter_limit = 40

let blotter (day : Trading_day.t) (rows : Replay.Blotter_row.t list) =
  let shown = List.take rows blotter_limit in
  let overflow = List.length rows - List.length shown in
  let table_rows =
    List.map shown ~f:(fun (row : Replay.Blotter_row.t) ->
      let side_class, side_text =
        match row.side with
        | Side.Buy -> "num good", "BUY"
        | Side.Sell -> "num bad", "SELL"
      in
      let order_type =
        match row.order_type with
        | Order_type.Market -> "MKT"
        | Order_type.Limit price -> [%string "LMT %{Fmt.price price}"]
      in
      let status = Replay.Blotter_row.Status.label row.status in
      {%html|
        <tr>
          <td class="mono muted">#{bar_time day row.submitted_minute}</td>
          <td class="mono">#{Order_id.to_string row.id}</td>
          <td class=%{side_class}>#{side_text}</td>
          <td class="mono">#{order_type}</td>
          <td class="num">#{Fmt.shares_int row.quantity}</td>
          <td class="num">#{Fmt.shares_int row.filled}</td>
          <td class="mono muted">#{status}</td>
        </tr>
      |})
  in
  let overflow_note =
    if overflow > 0
    then
      {%html|<div class="muted small mt-md">… and #{Int.to_string overflow} earlier orders</div>|}
    else Vdom.Node.none
  in
  {%html|
    <div class="panel mt-md">
      <div class="section-label">Child-order blotter</div>
      <div class="blotter">
        <table class="table">
          <thead>
            <tr>
              <th>Time</th><th>Id</th><th>Side</th><th>Type</th>
              <th class="num">Qty</th><th class="num">Filled</th><th>Status</th>
            </tr>
          </thead>
          <tbody>*{table_rows}</tbody>
        </table>
      </div>
      %{overflow_note}
    </div>
  |}
;;

let component
  ~(model : Model.t Bonsai.t)
  ~(update : Model.updater Bonsai.t)
  (local_ graph)
  =
  let output =
    let%arr { Model.output; _ } = model in
    output
  in
  (* The playback clock: one bar per tick, tick interval set by the speed.
     Registered inside this component, so it only runs on this screen. *)
  let span =
    let%arr { Model.sim_speed; _ } = model in
    Time_ns.Span.of_ms (1000. /. Float.of_int (Int.max 1 sim_speed))
  in
  let tick =
    let%arr update in
    update (fun m ->
      if not m.Model.sim_playing
      then m
      else (
        let next = m.Model.sim_minute + 1 in
        if next >= last_minute
        then { m with Model.sim_minute = last_minute; sim_playing = false }
        else { m with Model.sim_minute = next }))
  in
  Bonsai.Clock.every
    ~when_to_start_next_effect:`Every_multiple_of_period_non_blocking
    ~trigger_on_activate:false
    span
    tick
    graph;
  let events =
    let%arr output in
    match output with None -> [] | Some output -> Replay.events output
  in
  let day =
    let%arr output in
    Option.map output ~f:(fun o -> o.Sim.Output.day)
  in
  let fills =
    let%arr output in
    match output with None -> [] | Some output -> output.fills
  in
  let visible_upto =
    let%arr { Model.sim_minute; _ } = model in
    Some sim_minute
  in
  let on_seek =
    let%arr update in
    Some
      (fun minute ->
        update (fun m ->
          { m with
            Model.sim_minute = Int.clamp_exn minute ~min:0 ~max:last_minute
          }))
  in
  let chart = Chart.component ~visible_upto ~on_seek ~day ~fills graph in
  let%arr output
  and events
  and chart
  and { Model.sim_minute; sim_playing; sim_speed; _ } = model
  and update in
  match output with
  | None ->
    let start =
      on_click
        (update (fun m -> { m with Model.screen = Model.Screen.Choose_day }))
    in
    {%html|
      <div class="content">
        <h1 class="page-title">Simulator</h1>
        <div class="callout">No run to replay yet.</div>
        <div class="mt-lg"><button class="btn primary" %{start}>Set one up</button></div>
      </div>
    |}
  | Some output ->
    let day = output.day in
    let parent_views = Replay.Parent_view.at output ~minute:sim_minute in
    let blotter_rows = Replay.Blotter_row.at output ~minute:sim_minute in
    let filled =
      List.sum (module Int) parent_views ~f:(fun v ->
        v.Replay.Parent_view.filled)
    in
    let requested =
      List.sum (module Int) parent_views ~f:(fun v ->
        v.Replay.Parent_view.quantity)
    in
    let fill_count =
      List.count output.fills ~f:(fun fill ->
        Replay.minute_of_ofday fill.Fill.time <= sim_minute)
    in
    let working =
      List.count blotter_rows ~f:(fun row ->
        match row.Replay.Blotter_row.status with
        | Replay.Blotter_row.Status.Live -> true
        | Replay.Blotter_row.Status.Filled
        | Replay.Blotter_row.Status.Canceled (_ : Cancel_reason.t) ->
          false)
    in
    let title =
      [%string
        "%{Symbol.to_string day.Trading_day.symbol} %{Fmt.date \
         day.Trading_day.date} · %{output.algo_name} · live replay"]
    in
    let results =
      on_click
        (update (fun m -> { m with Model.screen = Model.Screen.Results }))
    in
    let results_label =
      if sim_minute >= last_minute then "View results" else "Skip to results"
    in
    let back =
      on_click
        (update (fun m -> { m with Model.screen = Model.Screen.Confirm }))
    in
    let stat_tiles =
      [ Screens.Tile.view
          ~label:"Filled"
          ~value:(Fmt.shares_int filled)
          ~sub:[%string "of %{Fmt.shares_int requested} shares"]
          ()
      ; Screens.Tile.view
          ~label:"Fills"
          ~value:(Int.to_string fill_count)
          ~sub:"executions so far"
          ()
      ; Screens.Tile.view
          ~label:"Working orders"
          ~value:(Int.to_string working)
          ~sub:"live in the market"
          ()
      ]
    in
    {%html|
      <div class="content wide">
        <h1 class="page-title">#{title}</h1>
        %{toolbar day ~minute:sim_minute ~playing:sim_playing
            ~speed:sim_speed ~update}
        <div class="row mt-md">*{stat_tiles}</div>
        <div class="mt-md">
          %{chart}
          <div class="muted small mt-md">Tip — click anywhere on the chart to jump the clock there.</div>
        </div>
        <div class="sim-grid mt-md">
          %{parent_panel parent_views}
          %{event_log day events ~minute:sim_minute}
        </div>
        %{blotter day blotter_rows}
        <div class="row mt-xl">
          <button class="btn primary" %{results}>#{results_label}</button>
          <button class="btn ghost" %{back}>Back to review</button>
        </div>
      </div>
    |}
;;
