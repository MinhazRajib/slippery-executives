open! Core
open! Bonsai_web
open Bonsai.Let_syntax
open! Execlab_types
open! Execlab_market
open! Execlab_analytics

(* Wizard flow (adapted from app/ui/client's seven screens): Dashboard ->
   Choose_day -> Alpha -> Setup (algorithm + confirm) -> Sim -> Results. *)
module Screen = struct
  type t =
    | Landing (** the marketing front door; not part of the wizard *)
    | Dashboard
    | My_runs (** the signed-in user's execution notebook *)
    | Choose_day
    | Alpha
    | Setup
    | Sim
    | Results
  [@@deriving sexp, equal]
end

let fs = sprintf "%.1f"

let side_str side =
  match (side : Side.t) with Buy -> "BUY" | Sell -> "SELL"
;;

(* Side-adjusted bps vs a benchmark: positive means worse (paid more /
   received less), so red is always bad and green always good. *)
let bps_vs ~(side : Side.t) ~avg ~benchmark =
  Float.of_int (Side.sign side) *. (avg -. benchmark) /. benchmark *. 10000.
;;

(* Signed cost convention: positive = worse (paid more / received less),
   negative = better; color carries the same signal. *)
let bps_view ~theme value =
  let color =
    if Float.( <= ) value 0. then theme.Styles.green else theme.Styles.red
  in
  let style =
    Styles.s
      ("color:" ^ color ^ ";font-size:13px;font-weight:600;" ^ Styles.mono)
  in
  let unit_style =
    Styles.s ("color:" ^ theme.Styles.faint ^ ";font-size:11px;")
  in
  {%html|<span %{style}>#{sprintf "%+.1f" value} <span %{unit_style}>bps</span></span>|}
;;

let hhmm ofday = String.prefix (Time_ns.Ofday.to_string ofday) 5

let dollars_signed cents =
  (if cents < 0 then "-$" else "+$")
  ^ Float.to_string_hum
      ~delimiter:','
      ~decimals:2
      (Float.abs (Float.of_int cents /. 100.))
;;

(* Money readout colored by sign: profit green, loss red. *)
let money_stat ~theme ~label:text value_cents =
  let color =
    if value_cents > 0
    then theme.Styles.green
    else if value_cents < 0
    then theme.Styles.red
    else theme.Styles.secondary
  in
  let label_style =
    Styles.s ("color:" ^ theme.Styles.faint ^ ";font-size:12px;")
  in
  let value_style =
    Styles.s
      ("color:" ^ color ^ ";font-size:13px;font-weight:600;" ^ Styles.mono)
  in
  let pair = Styles.s "display:inline-flex;gap:6px;align-items:baseline;" in
  {%html|
    <span %{pair}>
      <span %{label_style}>#{text}</span>
      <span %{value_style}>#{dollars_signed value_cents}</span>
    </span>
  |}
;;

(* The one persistent navigation affordance: your dashboard, from any screen,
   always in the same corner. *)
let my_dashboard_button ~theme ~on_click =
  let style =
    Styles.s
      ("display:inline-flex;align-items:center;gap:7px;background:"
       ^ theme.Styles.blue_soft
       ^ ";color:"
       ^ theme.Styles.blue
       ^ ";border:1px solid "
       ^ theme.Styles.chip_border
       ^ ";border-radius:999px;padding:6px \
          13px;cursor:pointer;font-size:12.5px;font-weight:700;white-space:nowrap;"
      )
  in
  {%html|
    <button
      class="btn"
      %{style}
      title="My dashboard"
      on_click=%{fun _ -> on_click}>
      My dashboard
    </button>
  |}
;;

(* Light/dark switch; lives in each screen's header. *)
let theme_button ~theme ~is_dark ~toggle_theme =
  let style =
    Styles.s
      ("background:transparent;color:"
       ^ theme.Styles.secondary
       ^ ";border:1px solid "
       ^ theme.Styles.chip_border
       ^ ";border-radius:3px;padding:4px \
          10px;cursor:pointer;font-size:11.5px;font-weight:600;white-space:nowrap;"
       ^ Styles.mono)
  in
  {%html|
    <button %{style} on_click=%{fun _ -> toggle_theme}>
      #{if is_dark then "◑ light" else "◐ dark"}
    </button>
  |}
;;

(* The brand lockup from the mockup: a bold wordmark with a small-caps mono
   suffix, the same on every screen. Given [on_click] it renders as the way
   home; on the landing page — which is home — it stays inert. *)
let wordmark ?on_click ~theme () =
  let name =
    Styles.s
      ("color:"
       ^ theme.Styles.blue
       ^ ";font-size:22px;font-weight:800;letter-spacing:-0.015em;")
  in
  let lockup = {%html|<span %{name}>ExecLab</span>|} in
  match on_click with
  | None -> lockup
  | Some effect ->
    let chrome =
      Styles.s "background:none;border:none;padding:0;cursor:pointer;"
    in
    {%html|
      <button
        class="btn"
        %{chrome}
        title="ExecLab home"
        on_click=%{fun _ -> effect}>
        %{lockup}
      </button>
    |}
;;

(* Icons come from Bonsai's own component library rather than hand-rolled SVG
   paths: same glyph set the rest of the Bonsai ecosystem uses, and the
   stroke follows [currentColor] so a chip's own text color drives it. *)
module Icon = struct
  let make ?(size = 15) (icon : Feather_icon.t) =
    Feather_icon.svg
      ~size:(`Px size)
      ~stroke:(`Name "currentColor")
      ~stroke_width:(`Px_float 2.2)
      ~extra_attrs:[ Styles.s "flex-shrink:0;vertical-align:-2px;" ]
      icon
  ;;

  let calendar ?size () = make ?size Calendar
  let play ?size () = make ?size Play
  let pause ?size () = make ?size Pause
  let upload ?size () = make ?size Upload
  let arrow_right ?size () = make ?size Arrow_right
  let arrow_left ?size () = make ?size Arrow_left
  let search ?size () = make ?size Search
  let zoom_in ?size () = make ?size Zoom_in
  let zoom_out ?size () = make ?size Zoom_out
end

let wizard_steps = [ "Day"; "Alpha"; "Setup"; "Replay"; "Results" ]

(* The mockup's numbered progress rail: "✓ 01 Day ─── 02 Alpha ───", hairline
   connectors stretching between stations, the active station in blue with an
   underline, completed stations checked off. *)
let step_progress ~theme ~current =
  let station index name =
    let state =
      if index < current
      then `Done
      else if index = current
      then `Active
      else `Upcoming
    in
    let color =
      match state with
      | `Done -> theme.Styles.secondary
      | `Active -> theme.Styles.blue
      | `Upcoming -> theme.Styles.faint
    in
    let underline =
      match state with
      | `Active -> "border-bottom:2px solid " ^ theme.Styles.blue ^ ";"
      | `Done | `Upcoming -> "border-bottom:2px solid transparent;"
    in
    let chip =
      Styles.s
        ("display:inline-flex;align-items:center;gap:6px;color:"
         ^ color
         ^ ";padding:2px 1px \
            4px;font-size:11px;font-weight:600;letter-spacing:0.08em;white-space:nowrap;"
         ^ underline
         ^ Styles.mono)
    in
    let check =
      match state with
      | `Done -> [ {%html|<span>✓</span>|} ]
      | `Active | `Upcoming -> []
    in
    {%html|
      <span %{chip}>
        *{check}
        <span>#{sprintf "%02d" (index + 1)}</span>
        <span>#{name}</span>
      </span>
    |}
  in
  let connector =
    Styles.s
      ("flex:1;height:1px;background:"
       ^ theme.Styles.hairline
       ^ ";min-width:16px;")
  in
  let stations =
    List.concat_mapi wizard_steps ~f:(fun index step ->
      let chip = station index step in
      if index = 0
      then [ chip ]
      else [ {%html|<span %{connector}></span>|}; chip ])
  in
  {%html|
    <div
      %{Styles.s
          "display:flex;align-items:center;gap:12px;margin-top:14px;width:100%;"}>
      *{stations}
    </div>
  |}
;;

(* ---------- controls ---------- *)

let pill ~theme ~active ~on_click label =
  let bg = if active then theme.Styles.blue else "transparent" in
  let color = if active then "#ffffff" else theme.Styles.secondary in
  let style =
    Styles.s
      ("background:"
       ^ bg
       ^ ";color:"
       ^ color
       ^ ";border:none;border-radius:3px;padding:5px \
          11px;cursor:pointer;font-size:12px;font-weight:600;"
       ^ Styles.mono)
  in
  {%html|<button %{style} on_click=%{on_click}>#{label}</button>|}
;;

(* "TSLA", or "AAPL +2": one line's worth of what a run traded. *)
(* What an alpha actually trades, for the wizard headers. The symbol a user
   browsed to get here is not the run's — the file decides that — so the
   headers read the file rather than the calendar. *)
let alpha_symbols_line alpha_text =
  match Replay.parse_alpha alpha_text with
  | Error (_ : Error.t) -> "—"
  | Ok [] -> "—"
  | Ok instructions ->
    List.map instructions ~f:(fun instruction ->
      instruction.Alpha_instruction.symbol)
    |> List.dedup_and_sort ~compare:Symbol.compare
    |> List.map ~f:Symbol.to_string
    |> String.concat ~sep:" "
;;

let chart_w = 1140.
let chart_left = 52.
let chart_right = 20.
let chart_plot_w = chart_w -. chart_left -. chart_right

module Chart_view = struct
  type t =
    | Follow of int option
    | Manual of
        { z0 : int
        ; z1 : int
        }
  [@@deriving equal]
end

(* The narrowest window we allow: below this the axis has nothing left to say
   and the fan of fills stops meaning anything. *)
let min_zoom_minutes = 6

(* Clamp a proposed window into the session, preserving its span. *)
let clamp_window ~n ~z0 ~span =
  let span = Int.max min_zoom_minutes (Int.min span (n - 1)) in
  let z0 = Int.max 0 (Int.min z0 (n - 1 - span)) in
  z0, z0 + span
;;

(* The cursor's position across the plot area, 0 at the left edge and 1 at
   the right; [None] when it is outside. *)
let plot_ratio_of_mouse
  (evt : Js_of_ocaml.Dom_html.mouseEvent Js_of_ocaml.Js.t)
  =
  let open Js_of_ocaml in
  match Js.Opt.to_option evt##.currentTarget with
  | None -> None
  | Some target ->
    let rect = target##getBoundingClientRect in
    let x_px = Js.to_float evt##.clientX -. Js.to_float rect##.left in
    let width = Float.of_int target##.clientWidth in
    if Float.( <= ) width 0.
    then None
    else (
      let svg_x = x_px *. (chart_w /. width) in
      let ratio = (svg_x -. chart_left) /. chart_plot_w in
      if Float.( < ) ratio 0. || Float.( > ) ratio 1.
      then None
      else Some ratio)
;;

let controls
  (replay : Replay.t)
  ~theme
  ~minute
  ~playing
  ~speed
  ~chart_view
  ~set_chart_view
  ~zoom_mode
  ~set_zoom_mode
  ~zoom_tool
  ~set_zoom_tool
  ~set_playing
  ~set_speed
  ~set_minute
  ~restart
  =
  let last = Replay.last_minute replay in
  let complete = minute >= last in
  let primary =
    Styles.s
      ("background:"
       ^ theme.Styles.brown
       ^ ";color:#ffffff;border:none;border-radius:3px;padding:8px \
          16px;cursor:pointer;font-size:13px;font-weight:700;white-space:nowrap;"
      )
  in
  let group =
    Styles.s
      ("display:flex;gap:2px;background:"
       ^ theme.Styles.chip_bg
       ^ ";border-radius:5px;padding:2px;")
  in
  (* Play/pause is the control people reach for first, so it gets its own
     shape: a wide button with the matching glyph and word, not a pill that
     looks like the speed options next to it. *)
  let play_button =
    let is_playing = playing && not complete in
    let style =
      Styles.s
        ("display:inline-flex;align-items:center;gap:7px;background:"
         ^ (if is_playing then theme.Styles.chip_bg else theme.Styles.blue)
         ^ ";color:"
         ^ (if is_playing then theme.Styles.text else theme.Styles.page_bg)
         ^ ";border:1px solid "
         ^ (if is_playing
            then theme.Styles.chip_border
            else theme.Styles.blue)
         ^ ";border-radius:8px;padding:9px \
            16px;cursor:pointer;font-size:13.5px;font-weight:700;min-width:104px;justify-content:center;"
        )
    in
    let glyph =
      if is_playing then Icon.pause ~size:14 () else Icon.play ~size:14 ()
    in
    {%html|
      <button
        class="btn"
        %{style}
        title=%{if is_playing then "Pause" else "Play"}
        on_click=%{fun _ -> set_playing (not playing)}>
        %{glyph}
        #{if is_playing then "Pause" else "Play"}
      </button>
    |}
  in
  let zoom_pill target label =
    pill
      ~theme
      ~active:(Chart_view.equal chart_view target)
      ~on_click:(fun _ -> set_chart_view target)
      label
  in
  (* Zoom mode is a stated mode, not a hidden gesture: the button lights up,
     the cursor changes, and the wheel stops scrolling the page. *)
  let magnifier =
    let style =
      Styles.s
        ("display:inline-flex;align-items:center;gap:6px;background:"
         ^ (if zoom_mode then theme.Styles.blue else theme.Styles.chip_bg)
         ^ ";color:"
         ^ (if zoom_mode
            then theme.Styles.page_bg
            else theme.Styles.secondary)
         ^ ";border:1px solid "
         ^ (if zoom_mode then theme.Styles.blue else theme.Styles.chip_border)
         ^ ";border-radius:7px;padding:7px \
            12px;cursor:pointer;font-size:12.5px;font-weight:700;white-space:nowrap;"
        )
    in
    {%html|
      <button
        class="btn"
        %{style}
        title="Zoom mode: scroll to zoom at the cursor, drag to pan"
        on_click=%{fun _ ->
          if zoom_mode
          then Effect.Many [ set_zoom_mode false; set_zoom_tool None ]
          else set_zoom_mode true}>
        %{Icon.search ~size:14 ()}
        #{if zoom_mode then "Zoom on" else "Zoom"}
      </button>
    |}
  in
  let slider_style =
    Styles.s
      ("flex:1 1 220px;accent-color:"
       ^ theme.Styles.brown
       ^ ";min-width:140px;")
  in
  let clock_style =
    Styles.s
      ("color:"
       ^ theme.Styles.text
       ^ ";font-size:15px;font-weight:700;"
       ^ Styles.mono)
  in
  (* The magnifier buttons arm a click tool rather than acting once: click
     the chart to zoom exactly there, as many times as you like; click the
     button again to disarm. Arming also switches zoom mode on so the rest of
     the machinery (cursor, wheel, pan) is consistent. *)
  let tool_button direction ~title_ ~glyph =
    let armed =
      match zoom_tool, direction with
      | Some `In, `In | Some `Out, `Out -> true
      | (Some (`In | `Out) | None), (`In | `Out) -> false
    in
    let style =
      Styles.s
        ("display:inline-flex;align-items:center;justify-content:center;background:"
         ^ (if armed then theme.Styles.blue else theme.Styles.chip_bg)
         ^ ";color:"
         ^ (if armed then theme.Styles.page_bg else theme.Styles.secondary)
         ^ ";border:1px solid "
         ^ (if armed then theme.Styles.blue else theme.Styles.chip_border)
         ^ ";border-radius:7px;width:32px;height:32px;cursor:pointer;")
    in
    {%html|
      <button
        class="btn"
        %{style}
        title=%{title_}
        on_click=%{fun _ ->
          if armed
          then set_zoom_tool None
          else
            Effect.Many
              [ set_zoom_tool (Some direction); set_zoom_mode true ]}>
        %{glyph}
      </button>
    |}
  in
  (* Everything about looking at the chart in one group: turn zoom on, step
     in and out, read the window, put it back. *)
  let zoom_cluster =
    let is_default = Chart_view.equal chart_view (Chart_view.Follow None) in
    let reset_button =
      let style =
        Styles.s
          ("background:"
           ^ theme.Styles.chip_bg
           ^ ";color:"
           ^ (if is_default then theme.Styles.faint else theme.Styles.blue)
           ^ ";border:1px solid "
           ^ theme.Styles.chip_border
           ^ ";border-radius:7px;padding:8px \
              11px;font-size:12px;font-weight:700;cursor:pointer;")
      in
      let disabled =
        if is_default then Vdom.Attr.disabled else Vdom.Attr.empty
      in
      {%html|
        <button
          class="btn"
          %{style}
          %{disabled}
          title="Show the whole session"
          on_click=%{fun _ -> set_chart_view (Chart_view.Follow None)}>
          Reset
        </button>
      |}
    in
    {%html|
      <span %{Styles.s "display:flex;gap:6px;align-items:center;"}>
        %{magnifier}
        %{tool_button `In ~title_:"Zoom in where you click"
            ~glyph:(Icon.zoom_in ~size:15 ())}
        %{tool_button `Out ~title_:"Zoom out where you click"
            ~glyph:(Icon.zoom_out ~size:15 ())}
        %{reset_button}
      </span>
    |}
  in
  let status_text =
    if complete
    then "session complete"
    else if playing
    then "replaying"
    else "paused"
  in
  (* The mockup's "14:22 REPLAYING" readout: small caps, amber while the tape
     runs, green once the session completes. *)
  let status_style =
    Styles.s
      ("color:"
       ^ (if complete
          then theme.Styles.green
          else if playing
          then theme.Styles.brown
          else theme.Styles.faint)
       ^ ";font-size:10.5px;font-weight:700;letter-spacing:0.12em;text-transform:uppercase;white-space:nowrap;"
       ^ Styles.mono)
  in
  let row =
    Styles.s
      "display:flex;align-items:center;gap:14px;padding:12px \
       16px;flex-wrap:wrap;"
  in
  let on_slide (_ : _) value =
    match Int.of_string_opt value with
    | Some v -> set_minute (fun (_ : int) -> Int.max 0 (Int.min last v))
    | None -> Effect.Ignore
  in
  {%html|
    <div %{Styles.card theme ""}>
      <div %{row}>
        <button %{primary} on_click=%{fun _ -> restart}>↻ Replay day</button>
        <div %{group}>
          %{play_button}
          %{pill ~theme ~active:(speed = 1)
              ~on_click:(fun _ -> set_speed 1) "1x"}
          %{pill ~theme ~active:(speed = 4)
              ~on_click:(fun _ -> set_speed 4) "4x"}
          %{pill ~theme ~active:(speed = 16)
              ~on_click:(fun _ -> set_speed 16) "16x"}
        </div>
        <div %{group}>
          %{zoom_pill (Chart_view.Follow None) "Day"}
          %{zoom_pill (Chart_view.Follow (Some 120)) "2h"}
          %{zoom_pill (Chart_view.Follow (Some 60)) "1h"}
          %{zoom_pill (Chart_view.Follow (Some 30)) "30m"}
          %{zoom_pill (Chart_view.Follow (Some 15)) "15m"}
        </div>
        %{zoom_cluster}
        <input
          type="range"
          min=%{0.}
          max=%{Float.of_int last}
          value=%{Int.to_string minute}
          on_input=%{on_slide}
          %{slider_style} />
        <span %{clock_style}>#{Replay.clock_string replay ~minute}</span>
        <span %{status_style}>#{status_text}</span>
      </div>
    </div>
  |}
;;

(* ---------- the chart: price line, order windows, crosshair ---------- *)

(* Geometry shared by the renderer and the mouse-position inverse. *)

(* The hovered minute, from a mouse event over the chart svg: CSS pixels ->
   viewBox units -> bar index, clamped to the replayed range. *)
(* Where the chart is looking. [Follow] tracks the playhead with a preset
   span (None = the whole session); [Manual] is an explicit window the user
   reached by wheel-zooming or dragging, and stops following. *)
let minute_of_mouse
  ~view:(z0, z1)
  ~shown
  (evt : Js_of_ocaml.Dom_html.mouseEvent Js_of_ocaml.Js.t)
  =
  let open Js_of_ocaml in
  match Js.Opt.to_option evt##.currentTarget with
  | None -> None
  | Some target ->
    let rect = target##getBoundingClientRect in
    let x_px = Js.to_float evt##.clientX -. Js.to_float rect##.left in
    let width = Float.of_int target##.clientWidth in
    if Float.( <= ) width 0.
    then None
    else (
      let svg_x = x_px *. (chart_w /. width) in
      let ratio = (svg_x -. chart_left) /. chart_plot_w in
      if Float.( < ) ratio 0. || Float.( > ) ratio 1.
      then None
      else
        Some
          (Int.max
             z0
             (Int.min
                (Int.min shown z1)
                (z0
                 + Float.iround_nearest_exn
                     (ratio *. Float.of_int (Int.max 1 (z1 - z0)))))))
;;

let chart
  (replay : Replay.t)
  ~theme
  ~focus
  ~minute
  ~fills
  ~show_fills
  ~hover
  ~set_hover
  ~view:(z0, z1)
  ~zoom_mode
  ~zoom_tool
  ~set_view
  ~drag
  ~set_drag
  =
  (* A price axis belongs to one stock, so the chart draws the focused symbol
     only: its bars, its orders, its fills. Order numbers stay global (O3 is
     the third line of the alpha whichever name it is), so the chart and the
     tables agree. *)
  let bars = Replay.bars_for replay focus in
  let focused =
    List.filter_mapi replay.parents ~f:(fun index parent ->
      if Symbol.equal parent.Replay.symbol focus
      then Some (index, parent)
      else None)
  in
  let fills =
    List.filter fills ~f:(fun fill -> Symbol.equal fill.Fill.symbol focus)
  in
  let n = Array.length bars in
  let w = chart_w in
  let left = chart_left in
  let plot_w = chart_plot_w in
  let top = 10. in
  let price_h = 430. in
  let axis_y = top +. price_h +. 18. in
  let h = axis_y +. 6. in
  (* The y scale fits the *visible* window, so zooming in also zooms the
     price axis. *)
  let visible = Array.sub bars ~pos:z0 ~len:(z1 - z0 + 1) in
  let lo =
    Array.fold visible ~init:Float.infinity ~f:(fun acc bar ->
      Float.min acc (Price.to_float bar.Market_bar.low))
  in
  let hi =
    Array.fold visible ~init:Float.neg_infinity ~f:(fun acc bar ->
      Float.max acc (Price.to_float bar.Market_bar.high))
  in
  (* Pad the price domain so the line breathes instead of grazing the top and
     bottom edges of the plot. *)
  let lo, hi =
    let raw = Float.max (hi -. lo) 0.01 in
    let pad = raw *. 0.08 in
    lo -. pad, hi +. pad
  in
  let span = Float.max (hi -. lo) 0.01 in
  let x i =
    left
    +. (Float.of_int (i - z0) /. Float.of_int (Int.max 1 (z1 - z0)) *. plot_w)
  in
  let y v = top +. ((hi -. v) /. span *. price_h) in
  let shown_end = Int.min minute z1 in
  let svg name attrs children = Vdom.Node.create_svg name ~attrs children in
  let attr = Vdom.Attr.create in
  let tooltip text = svg "title" [] [ Vdom.Node.text text ] in
  let hover =
    Option.map hover ~f:(fun m -> Int.max z0 (Int.min m shown_end))
  in
  let hovered_in (parent : Replay.parent_replay) =
    match hover with
    | None -> false
    | Some m -> m >= parent.arrival_minute && m <= parent.deadline_minute
  in
  (* horizontal gridlines at round price levels; finer steps once zoomed *)
  let grid =
    let step =
      if Float.( < ) span 2.5
      then 0.5
      else Float.max 1. (Float.round_up (span /. 5.))
    in
    let price_label v =
      if Float.( < ) step 1. then sprintf "$%.2f" v else sprintf "$%.0f" v
    in
    let start = Float.round_up (lo /. step) *. step in
    let rec levels v acc =
      if Float.( > ) v hi then acc else levels (v +. step) (v :: acc)
    in
    List.concat_map (levels start []) ~f:(fun v ->
      [ svg
          "line"
          [ attr "x1" (fs left)
          ; attr "x2" (fs (left +. plot_w))
          ; attr "y1" (fs (y v))
          ; attr "y2" (fs (y v))
          ; attr "stroke" theme.Styles.hairline
          ; attr "stroke-width" "1"
          ]
          []
      ; svg
          "text"
          [ attr "x" (fs (left -. 8.))
          ; attr "y" (fs (y v +. 3.))
          ; attr "text-anchor" "end"
          ; attr "fill" theme.Styles.faint
          ; attr "font-size" "11"
          ]
          [ Vdom.Node.text (price_label v) ]
      ])
  in
  (* Windows are drawn as full-height translucent bands and simply allowed to
     overlap: two bands stacking compound their alpha, so an interval where
     several orders are live reads as a visibly denser shade without any
     stacking, offsetting or lane logic. Each band also gets a solid top rail
     in its own order color, which survives the blend and keeps identity
     legible no matter how many bands pile up. *)
  let window_visible (parent : Replay.parent_replay) =
    parent.arrival_minute <= z1 && parent.deadline_minute >= z0
  in
  let window_edges (parent : Replay.parent_replay) =
    ( x (Int.max z0 parent.arrival_minute)
    , x (Int.min z1 parent.deadline_minute) )
  in
  let window_tooltip index (parent : Replay.parent_replay) =
    tooltip
      (sprintf
         "order %d %s %s → %s · arrival %s"
         (index + 1)
         (side_str parent.instruction.Alpha_instruction.side)
         (hhmm parent.instruction.Alpha_instruction.arrival_time)
         (hhmm parent.instruction.Alpha_instruction.deadline)
         (Price.to_string_dollar parent.arrival_price))
  in
  let windows =
    List.concat_map focused ~f:(fun (index, parent) ->
      if not (window_visible parent)
      then []
      else (
        let color = Styles.order_color theme index in
        let x0, x1 = window_edges parent in
        let width = Float.max 2. (x1 -. x0) in
        let rail_y = top +. 3. +. (Float.of_int index *. 3.5) in
        [ svg
            "rect"
            [ attr "x" (fs x0)
            ; attr "y" (fs top)
            ; attr "width" (fs width)
            ; attr "height" (fs price_h)
            ; attr "fill" color
            ; attr
                "fill-opacity"
                (if hovered_in parent then "0.22" else "0.13")
            ]
            [ window_tooltip index parent ]
        ; svg
            "rect"
            [ attr "x" (fs x0)
            ; attr "y" (fs rail_y)
            ; attr "width" (fs width)
            ; attr "height" (if hovered_in parent then "3" else "2.2")
            ; attr "rx" "1"
            ; attr "fill" color
            ]
            [ window_tooltip index parent ]
        ]
        (* The window says what it is, on the window. A legend listing every
           parent order stops fitting the moment an alpha file has more than
           a handful; a tag anchored to its own band never does. *)
        @
        let label =
          sprintf
            "O%d %s %s"
            (index + 1)
            (side_str parent.instruction.Alpha_instruction.side)
            (Int.to_string_hum
               ~delimiter:','
               (Size.to_int parent.instruction.Alpha_instruction.quantity))
        in
        let short = sprintf "O%d" (index + 1) in
        let text =
          if Float.( > ) width 108.
          then label
          else if Float.( > ) width 26.
          then short
          else ""
        in
        if String.is_empty text
        then []
        else (
          let tag_w = (Float.of_int (String.length text) *. 6.4) +. 12. in
          let tag_y = rail_y +. 5.5 in
          [ svg
              "rect"
              [ attr "x" (fs (x0 +. 2.))
              ; attr "y" (fs tag_y)
              ; attr "width" (fs tag_w)
              ; attr "height" "15"
              ; attr "rx" "3"
              ; attr "fill" color
              ; attr "fill-opacity" "0.92"
              ]
              [ window_tooltip index parent ]
          ; svg
              "text"
              [ attr "x" (fs (x0 +. 8.))
              ; attr "y" (fs (tag_y +. 11.))
              ; attr "fill" theme.Styles.card_bg
              ; attr "font-size" "10.5"
              ; attr "font-weight" "700"
              ]
              [ Vdom.Node.text text ]
          ])))
  in
  (* Arrival reference lines stay, but only when few enough to read. *)
  let arrival_lines =
    if List.length focused > 4
    then []
    else
      List.concat_map focused ~f:(fun (index, parent) ->
        if not (window_visible parent)
        then []
        else (
          let x0, x1 = window_edges parent in
          let arrival = Price.to_float parent.arrival_price in
          if Float.( < ) arrival lo || Float.( > ) arrival hi
          then []
          else
            [ svg
                "line"
                [ attr "x1" (fs x0)
                ; attr "x2" (fs x1)
                ; attr "y1" (fs (y arrival))
                ; attr "y2" (fs (y arrival))
                ; attr "stroke" (Styles.order_color theme index)
                ; attr "stroke-width" "1"
                ; attr "stroke-opacity" "0.55"
                ; attr "stroke-dasharray" "4 3"
                ]
                []
            ]))
  in
  (* the whole-day vwap as a flat dashed reference line *)
  let day_vwap = (Replay.vwap_for replay focus).(n - 1) in
  let vwap_line =
    [ svg
        "line"
        [ attr "x1" (fs left)
        ; attr "x2" (fs (left +. plot_w))
        ; attr "y1" (fs (y day_vwap))
        ; attr "y2" (fs (y day_vwap))
        ; attr "stroke" theme.Styles.orange
        ; attr "stroke-width" "1.5"
        ; attr "stroke-dasharray" "5 4"
        ]
        []
    ; svg
        "text"
        [ attr "x" (fs (left +. plot_w -. 6.))
        ; attr "y" (fs (y day_vwap -. 5.))
        ; attr "text-anchor" "end"
        ; attr "fill" theme.Styles.orange
        ; attr "font-size" "11"
        ]
        [ Vdom.Node.text (sprintf "vwap %.2f" day_vwap) ]
    ]
  in
  (* price line up to the playhead (within the window), with an end dot *)
  let price_line =
    if shown_end < z0
    then []
    else (
      let pts =
        List.init
          (shown_end - z0 + 1)
          ~f:(fun offset ->
            let i = z0 + offset in
            sprintf
              "%s,%s"
              (fs (x i))
              (fs (y (Price.to_float bars.(i).Market_bar.close))))
        |> String.concat ~sep:" "
      in
      [ svg
          "polyline"
          [ attr "points" pts
          ; attr "fill" "none"
          ; attr "stroke" theme.Styles.blue
          ; attr "stroke-width" "1.8"
          ; attr "stroke-linejoin" "round"
          ]
          []
      ; svg
          "circle"
          [ attr "cx" (fs (x shown_end))
          ; attr
              "cy"
              (fs (y (Price.to_float bars.(shown_end).Market_bar.close)))
          ; attr "r" "4"
          ; attr "fill" theme.Styles.blue
          ]
          []
      ])
  in
  let time_axis =
    let window = z1 - z0 in
    let step =
      if window > 240
      then 60
      else if window > 120
      then 30
      else if window > 45
      then 15
      else 5
    in
    List.filter_map (List.init n ~f:Fn.id) ~f:(fun i ->
      if i % step = 0
         && i >= z0
         && i <= z1
         && Float.( > ) (x i) (left +. 14.)
         && Float.( < ) (x i) (left +. plot_w -. 14.)
      then
        Some
          (svg
             "text"
             [ attr "x" (fs (x i))
             ; attr "y" (fs axis_y)
             ; attr "text-anchor" "middle"
             ; attr "fill" theme.Styles.faint
             ; attr "font-size" "11"
             ]
             [ Vdom.Node.text (hhmm bars.(i).Market_bar.time) ])
      else None)
  in
  (* Every fill is drawn at its exact executed price and its exact bar. The
     engine is bar-granular, so several fills can share one minute; rather
     than aggregate them (which would hide work) they fan out *within* that
     bar's own pixel width, ordered by parent. At day zoom a bar is a few
     pixels wide so the fan is invisible and no false precision is implied;
     zoom in and the same fills separate cleanly. Radius grows with zoom for
     the same reason. *)
  let fill_dots =
    if not show_fills
    then []
    else (
      let bar_width = plot_w /. Float.of_int (Int.max 1 (z1 - z0)) in
      let radius = Float.min 4.2 (Float.max 2. (bar_width /. 3.)) in
      (* Group the visible fills by minute so each group can be fanned. *)
      let by_minute = Hashtbl.create (module Int) in
      List.iter fills ~f:(fun (fill : Fill.t) ->
        let m = Replay.minute_of_time replay fill.time in
        if m >= z0 && m <= z1
        then Hashtbl.add_multi by_minute ~key:m ~data:fill);
      Hashtbl.to_alist by_minute
      |> List.concat_map ~f:(fun (m, minute_fills) ->
        let minute_fills = List.rev minute_fills in
        let count = List.length minute_fills in
        (* The fan never exceeds the bar it belongs to. *)
        let spread =
          Float.min (bar_width *. 0.72) (Float.of_int count *. 3.4)
        in
        List.mapi minute_fills ~f:(fun i (fill : Fill.t) ->
          let index = Replay.parent_index_of_order replay fill.order_id in
          let offset =
            if count <= 1
            then 0.
            else
              (Float.of_int i /. Float.of_int (count - 1) *. spread)
              -. (spread /. 2.)
          in
          svg
            "circle"
            [ attr "cx" (fs (x m +. offset))
            ; attr "cy" (fs (y (Price.to_float fill.price)))
            ; attr "r" (fs radius)
            ; attr "fill" (Styles.order_color theme index)
            ; attr "stroke" theme.Styles.card_bg
            ; attr
                "stroke-width"
                (if Float.( > ) radius 3. then "1.1" else "0.7")
            ]
            [ (let bar = bars.(m) in
               let fill_price = Price.to_float fill.price in
               let bar_open = Price.to_float bar.Market_bar.open_ in
               let bar_close = Price.to_float bar.Market_bar.close in
               tooltip
                 (sprintf
                    "YOUR FILL: %s %d shares @ $%.2f (%s, order %d, %s)\n\
                     STOCK THAT MINUTE: opened $%.2f · closed $%.2f\n\
                     %s %+.2f vs the open"
                    (side_str fill.side)
                    (Size.to_int fill.size)
                    fill_price
                    (hhmm fill.time)
                    (index + 1)
                    (match fill.liquidity with
                     | Taker -> "taker"
                     | Maker -> "maker")
                    bar_open
                    bar_close
                    (match fill.side with
                     | Buy -> "you paid"
                     | Sell -> "you received")
                    (fill_price -. bar_open)))
            ])))
  in
  (* The Google-Finance-style crosshair: a dashed vertical at the hovered
     minute, a dot on the close, and a small panel of that bar's numbers
     (plus which order windows cover the moment). *)
  let crosshair =
    match hover with
    | None -> []
    | Some m ->
      let bar = bars.(m) in
      let cx = x m in
      let close = Price.to_float bar.Market_bar.close in
      let covering =
        List.filter_map focused ~f:(fun (index, parent) ->
          if m >= parent.arrival_minute && m <= parent.deadline_minute
          then
            Some
              ( Styles.order_color theme index
              , sprintf
                  "O%d %s window"
                  (index + 1)
                  (side_str parent.instruction.Alpha_instruction.side) )
          else None)
      in
      (* The fills that landed in this minute, with their exact prices —
         native SVG tooltips die under the crosshair's re-rendering, so the
         panel is where a fill's price actually gets read. *)
      let minute_fills =
        List.filter_map fills ~f:(fun (fill : Fill.t) ->
          if Replay.minute_of_time replay fill.time = m
          then (
            let index = Replay.parent_index_of_order replay fill.order_id in
            Some
              ( Styles.order_color theme index
              , sprintf
                  "O%d %s %d @ $%.2f"
                  (index + 1)
                  (side_str fill.side)
                  (Size.to_int fill.size)
                  (Price.to_float fill.price) ))
          else None)
      in
      let headline = sprintf "$%.2f · %s" close (hhmm bar.Market_bar.time) in
      let detail =
        sprintf
          "O %.2f  H %.2f  L %.2f  ·  vol %s"
          (Price.to_float bar.Market_bar.open_)
          (Price.to_float bar.Market_bar.high)
          (Price.to_float bar.Market_bar.low)
          (Int.to_string_hum
             ~delimiter:','
             (Size.to_int bar.Market_bar.volume))
      in
      (* Size the panel to its text (approximate char widths at 13px bold /
         11px regular) instead of a fixed box. *)
      let box_w =
        let width ~per_char text =
          Float.of_int (String.length text) *. per_char
        in
        List.fold
          (width ~per_char:7.4 headline
           :: width ~per_char:5.9 detail
           :: List.map
                (covering @ minute_fills)
                ~f:(fun ((_ : string), text) -> width ~per_char:6.3 text))
          ~init:0.
          ~f:Float.max
        +. 20.
      in
      let line_h = 15. in
      let box_h =
        40.
        +. (Float.of_int (List.length covering + List.length minute_fills)
            *. line_h)
      in
      let box_x =
        if Float.( > ) (cx +. 12. +. box_w) (left +. plot_w)
        then cx -. 12. -. box_w
        else cx +. 12.
      in
      let box_y = top +. 6. in
      let label
        ?(fill = theme.Styles.secondary)
        ?(size = "11")
        ?(weight = "400")
        ~tx
        ~ty
        content
        =
        svg
          "text"
          [ attr "x" (fs tx)
          ; attr "y" (fs ty)
          ; attr "fill" fill
          ; attr "font-size" size
          ; attr "font-weight" weight
          ]
          [ Vdom.Node.text content ]
      in
      [ svg
          "line"
          [ attr "x1" (fs cx)
          ; attr "x2" (fs cx)
          ; attr "y1" (fs top)
          ; attr "y2" (fs (top +. price_h))
          ; attr "stroke" theme.Styles.faint
          ; attr "stroke-width" "1"
          ; attr "stroke-dasharray" "3 3"
          ]
          []
      ; svg
          "circle"
          [ attr "cx" (fs cx)
          ; attr "cy" (fs (y close))
          ; attr "r" "3.5"
          ; attr "fill" theme.Styles.blue
          ; attr "stroke" theme.Styles.card_bg
          ; attr "stroke-width" "1.5"
          ]
          []
      ; svg
          "rect"
          [ attr "x" (fs box_x)
          ; attr "y" (fs box_y)
          ; attr "width" (fs box_w)
          ; attr "height" (fs box_h)
          ; attr "rx" "5"
          ; attr "fill" theme.Styles.card_bg
          ; attr "stroke" theme.Styles.chip_border
          ; attr "fill-opacity" "0.96"
          ]
          []
      ; label
          ~fill:theme.Styles.text
          ~size:"13"
          ~weight:"700"
          ~tx:(box_x +. 10.)
          ~ty:(box_y +. 17.)
          headline
      ; label ~tx:(box_x +. 10.) ~ty:(box_y +. 32.) detail
      ]
      @ List.mapi (covering @ minute_fills) ~f:(fun i (color, text) ->
        label
          ~fill:color
          ~weight:"600"
          ~tx:(box_x +. 10.)
          ~ty:(box_y +. 32. +. (Float.of_int (i + 1) *. line_h))
          text)
  in
  svg
    "svg"
    [ attr "viewBox" (sprintf "0 0 %s %s" (fs w) (fs h))
    ; Styles.s
        ("width:100%;display:block;cursor:"
         ^ (match zoom_tool with
            | Some `In -> "zoom-in"
            | Some `Out -> "zoom-out"
            | None -> if zoom_mode then "grab" else "crosshair")
         ^ ";")
    ; Vdom.Attr.on_mousemove (fun evt ->
        match drag with
        | Some (anchor_ratio, anchor_z0) when zoom_mode ->
          (* Panning: hold the minute under the cursor still by shifting the
             window against the drag. *)
          (match plot_ratio_of_mouse evt with
           | None -> Effect.Ignore
           | Some ratio ->
             let span = z1 - z0 in
             let shift =
               Float.iround_nearest_exn
                 ((anchor_ratio -. ratio) *. Float.of_int span)
             in
             set_view (clamp_window ~n ~z0:(anchor_z0 + shift) ~span))
        | Some (_ : float * int) | None ->
          set_hover (minute_of_mouse ~view:(z0, z1) ~shown:minute evt))
    ; Vdom.Attr.on_mousedown (fun evt ->
        if (not zoom_mode) || Option.is_some zoom_tool
        then Effect.Ignore
        else (
          match plot_ratio_of_mouse evt with
          | None -> Effect.Ignore
          | Some ratio -> set_drag (Some (ratio, z0))))
    ; Vdom.Attr.on_click (fun evt ->
        (* An armed magnifier zooms about the exact point you click — the
           clicked minute keeps its screen position — and stays armed so
           successive clicks keep going. *)
        match zoom_tool with
        | None -> Effect.Ignore
        | Some direction ->
          (match plot_ratio_of_mouse evt with
           | None -> Effect.Ignore
           | Some ratio ->
             let span = z1 - z0 in
             let cursor =
               z0 + Float.iround_nearest_exn (ratio *. Float.of_int span)
             in
             let factor = match direction with `In -> 0.55 | `Out -> 1.8 in
             let new_span =
               Int.max
                 min_zoom_minutes
                 (Int.min
                    (Float.iround_nearest_exn (Float.of_int span *. factor))
                    (n - 1))
             in
             let new_z0 =
               cursor
               - Float.iround_nearest_exn (ratio *. Float.of_int new_span)
             in
             set_view (clamp_window ~n ~z0:new_z0 ~span:new_span)))
    ; Vdom.Attr.on_mouseup (fun (_ : _) -> set_drag None)
    ; Vdom.Attr.on_wheel (fun evt ->
        if not zoom_mode
        then Effect.Ignore
        else (
          match
            plot_ratio_of_mouse
              (evt :> Js_of_ocaml.Dom_html.mouseEvent Js_of_ocaml.Js.t)
          with
          | None -> Effect.Ignore
          | Some ratio ->
            (* Zoom about the cursor: the minute under the pointer keeps its
               screen position, so the chart grows around what you are
               looking at rather than around the middle. *)
            let span = z1 - z0 in
            let cursor =
              z0 + Float.iround_nearest_exn (ratio *. Float.of_int span)
            in
            (* Gentle: repeated notches glide instead of leaping. *)
            let factor =
              if Float.( > ) (Js_of_ocaml.Js.to_float evt##.deltaY) 0.
              then 1.08
              else 1. /. 1.08
            in
            let new_span =
              Float.iround_nearest_exn (Float.of_int span *. factor)
            in
            let new_span =
              Int.max min_zoom_minutes (Int.min new_span (n - 1))
            in
            let new_z0 =
              cursor
              - Float.iround_nearest_exn (ratio *. Float.of_int new_span)
            in
            (* Stop the page from scrolling underneath the zoom. *)
            evt##preventDefault;
            set_view (clamp_window ~n ~z0:new_z0 ~span:new_span)))
    ; Vdom.Attr.on_mouseleave (fun (_ : _) ->
        Effect.Many [ set_drag None; set_hover None ])
    ]
    (grid
     @ windows
     @ arrival_lines
     @ vwap_line
     @ time_axis
     @ price_line
     @ fill_dots
     @ crosshair)
;;

let legend
  (replay : Replay.t)
  ~theme
  ~focus
  ~minute
  ~fills
  ~show_fills
  ~toggle_fills
  =
  let item ~color ~line label =
    let swatch =
      if line
      then
        Styles.s
          ("display:inline-block;width:14px;height:3px;border-radius:2px;background:"
           ^ color
           ^ ";vertical-align:middle;")
      else
        Styles.s
          ("display:inline-block;width:3px;height:12px;background:"
           ^ color
           ^ ";vertical-align:middle;")
    in
    let text_style =
      Styles.s ("color:" ^ theme.Styles.secondary ^ ";font-size:13px;")
    in
    {%html|<span><span %{swatch}></span> <span %{text_style}>#{label}</span></span>|}
  in
  (* No per-order entries: a legend that grows with the alpha file is a
     legend that stops fitting. Orders are tagged on their own windows, and
     each fill takes its parent's color. *)
  let order_items =
    [ item
        ~color:theme.Styles.faint
        ~line:false
        "fills, colored by their order (tagged on the chart)"
    ]
  in
  let row =
    Styles.s
      "display:flex;gap:18px;align-items:center;padding:12px 16px 0 16px;"
  in
  let toggle =
    let bg =
      if show_fills then theme.Styles.blue else theme.Styles.chip_bg
    in
    let color = if show_fills then "#ffffff" else theme.Styles.secondary in
    let style =
      Styles.s
        ("background:"
         ^ bg
         ^ ";color:"
         ^ color
         ^ ";border:none;border-radius:4px;padding:5px \
            12px;cursor:pointer;font-size:12px;font-weight:600;")
    in
    {%html|
      <button %{style} on_click=%{fun _ -> toggle_fills}>
        #{if show_fills then "Hide fills" else "Show fills"}
      </button>
    |}
  in
  let stats =
    (* Marked per name: a multi-symbol run has no single last price. *)
    let open_pnl =
      Replay.open_pnl_cents ~fills ~last_for:(fun symbol ->
        Replay.last_close replay ~symbol ~minute)
    in
    let stats_style =
      Styles.s "margin-left:auto;display:flex;gap:16px;align-items:center;"
    in
    (* The benefit vs immediate is a whole-day number; revealing it
       mid-replay would spoil the ending — the mockup says so out loud. *)
    let benefit =
      if minute >= Replay.last_minute replay
      then
        [ money_stat
            ~theme
            ~label:"Execution benefit"
            replay.results.total_value_add_cents
        ]
      else (
        let style =
          Styles.s
            ("color:" ^ theme.Styles.faint ^ ";font-size:11px;" ^ Styles.mono)
        in
        [ {%html|<span %{style}>execution benefit — withheld until close</span>|}
        ])
    in
    {%html|
      <span %{stats_style}>
        %{money_stat ~theme ~label:"Total P&L" open_pnl}
        *{benefit}
      </span>
    |}
  in
  {%html|
    <div %{row}>
      %{item ~color:theme.Styles.blue ~line:true
          (Symbol.to_string focus ^ " price (1-min close)")}
      %{item ~color:theme.Styles.orange ~line:true "day vwap"}
      *{order_items}
      %{stats}
      %{toggle}
    </div>
  |}
;;

(* One tab per symbol in the run, because the chart can only carry one price
   axis at a time. Single-name runs get no strip at all. *)
let symbol_tabs (replay : Replay.t) ~theme ~focus ~set_focus =
  match replay.symbols with
  | [] | [ _ ] -> None
  | symbols ->
    let row =
      Styles.s
        ("display:flex;gap:6px;align-items:center;padding:12px 16px 0 \
          16px;border-bottom:1px solid "
         ^ theme.Styles.hairline
         ^ ";padding-bottom:10px;")
    in
    let label = Styles.s (Styles.table_label theme ^ "margin-right:4px;") in
    (* Same segmented pill the day picker uses, so choosing a name means one
       thing everywhere in the app. *)
    let tab symbol =
      let selected = Symbol.equal symbol focus in
      let style =
        Styles.s
          ("border:1px solid "
           ^ (if selected
              then theme.Styles.blue
              else theme.Styles.chip_border)
           ^ ";background:"
           ^ (if selected then theme.Styles.blue else theme.Styles.card_bg)
           ^ ";color:"
           ^ (if selected then "#ffffff" else theme.Styles.secondary)
           ^ ";border-radius:3px;padding:6px \
              12px;font-size:12px;font-weight:700;cursor:pointer;"
           ^ Styles.mono)
      in
      {%html|
        <button class="btn" %{style} on_click=%{fun _ -> set_focus symbol}>
          #{Symbol.to_string symbol}
        </button>
      |}
    in
    Some
      {%html|
        <div %{row}>
          <span %{label}>chart</span>
          *{List.map symbols ~f:tab}
        </div>
      |}
;;

(* ---------- the orders table ---------- *)

(* One row per parent order: scales to many orders where a card per order
   would not. *)
let orders_table (replay : Replay.t) ~theme ~fills ~minute =
  (* The symbol column only earns its width when the alpha has more than one
     name in it. *)
  let multi = List.length replay.symbols > 1 in
  let columns =
    (if multi then "92px 68px " else "92px ")
    ^ "96px 116px 78px 96px 148px 62px 112px 112px 82px 1fr"
  in
  let row_base =
    "display:grid;grid-template-columns:"
    ^ columns
    ^ ";column-gap:10px;align-items:baseline;"
  in
  let title_style =
    Styles.s
      ("color:" ^ theme.Styles.text ^ ";font-size:14px;font-weight:600;")
  in
  let head_row =
    Styles.s
      (row_base
       ^ "padding:10px 16px 6px 16px;border-bottom:1px solid "
       ^ theme.Styles.hairline
       ^ ";"
       ^ Styles.table_label theme)
  in
  let now = Replay.time_at replay ~minute in
  let dash =
    let style = Styles.s ("color:" ^ theme.Styles.faint ^ ";") in
    {%html|<span %{style}>-</span>|}
  in
  let row index (parent : Replay.parent_replay) =
    let instruction = parent.instruction in
    let color = Styles.order_color theme index in
    let side = instruction.Alpha_instruction.side in
    (* Each order is benchmarked against its own session's day VWAP. *)
    let day_vwap =
      let vwap = Replay.vwap_for replay parent.symbol in
      vwap.(Array.length vwap - 1)
    in
    let total = Size.to_int instruction.Alpha_instruction.quantity in
    let mine =
      List.filter fills ~f:(fun (fill : Fill.t) ->
        Set.mem parent.order_ids fill.order_id)
    in
    let filled =
      List.sum (module Int) mine ~f:(fun fill -> Size.to_int fill.size)
    in
    let notional =
      List.sum (module Int) mine ~f:(fun fill -> Fill.notional_cents fill)
    in
    let completion = if total = 0 then 0. else filled // total *. 100. in
    let status, status_color =
      if Time_ns.Ofday.( < ) now instruction.Alpha_instruction.arrival_time
      then "Pending", theme.Styles.faint
      else if filled >= total
      then "Complete", theme.Styles.green
      else if Time_ns.Ofday.( > ) now instruction.Alpha_instruction.deadline
      then "Expired", theme.Styles.red
      else "Working", theme.Styles.blue
    in
    let avg =
      if filled = 0 then None else Some (notional // filled /. 100.)
    in
    let vs benchmark =
      match avg with
      | None -> dash
      | Some a -> bps_view ~theme (bps_vs ~side ~avg:a ~benchmark)
    in
    let chip =
      Styles.s
        ("display:inline-block;width:12px;height:3px;border-radius:2px;vertical-align:middle;background:"
         ^ color
         ^ ";margin-right:8px;")
    in
    let style =
      Styles.s
        (row_base
         ^ "padding:8px 16px;font-size:13px;color:"
         ^ theme.Styles.text
         ^ ";border-bottom:1px solid "
         ^ theme.Styles.hairline
         ^ ";"
         ^ Styles.mono)
    in
    let order_label =
      Styles.s ("font-weight:600;color:" ^ theme.Styles.text ^ ";")
    in
    let warm = Styles.s ("color:" ^ theme.Styles.brown ^ ";") in
    let bold = Styles.s "font-weight:600;" in
    let faint = Styles.s ("color:" ^ theme.Styles.faint ^ ";") in
    let status_style =
      Styles.s ("color:" ^ status_color ^ ";font-weight:600;")
    in
    let bar_outer =
      Styles.s
        ("display:block;height:4px;border-radius:2px;overflow:hidden;align-self:center;background:"
         ^ theme.Styles.chip_bg
         ^ ";")
    in
    let bar_inner =
      Styles.s
        (sprintf
           "display:block;height:100%%;width:%.1f%%;background:%s;"
           completion
           color)
    in
    let avg_view =
      match avg with
      | None -> dash
      | Some a ->
        let text = sprintf "$%.2f" a in
        {%html|<span>#{text}</span>|}
    in
    let symbol_cell =
      if not multi
      then []
      else (
        let style = Styles.s ("color:" ^ theme.Styles.text ^ ";") in
        [ {%html|<span %{style}>#{Symbol.to_string parent.symbol}</span>|} ])
    in
    {%html|
      <div %{style}>
        <span %{order_label}><span %{chip}></span>Order %{index + 1#Int}</span>
        *{symbol_cell}
        <span %{bold}>#{side_str side} #{Int.to_string_hum ~delimiter:',' total}</span>
        <span %{warm}>
          #{hhmm instruction.Alpha_instruction.arrival_time}
          →
          #{hhmm instruction.Alpha_instruction.deadline}
        </span>
        <span %{warm}>#{Price.to_string_dollar parent.arrival_price}</span>
        <span %{bold}>%{avg_view}</span>
        <span>
          #{Int.to_string_hum ~delimiter:',' filled}
          <span %{faint}>/ #{Int.to_string_hum ~delimiter:',' total}
            (#{sprintf "%.0f" completion}%)</span>
        </span>
        <span %{warm}>%{List.length mine#Int}/%{Set.length parent.order_ids#Int}</span>
        <span>%{vs (Price.to_float parent.arrival_price)}</span>
        <span>%{vs day_vwap}</span>
        <span %{status_style}>#{status}</span>
        <span %{bar_outer}><span %{bar_inner}></span></span>
      </div>
    |}
  in
  let header = Styles.s "padding:14px 16px 0 16px;" in
  let symbol_head = if multi then [ {%html|<span>symbol</span>|} ] else [] in
  {%html|
    <div %{Styles.card theme "padding-bottom:4px;"}>
      <div %{header}><span %{title_style}>Orders</span></div>
      <div %{head_row}>
        <span>order</span>
        *{symbol_head}
        <span>side · qty</span>
        <span>window</span>
        <span>arrival</span>
        <span>avg fill</span>
        <span>filled</span>
        <span>slices</span>
        <span>vs arrival</span>
        <span>vs day vwap</span>
        <span>status</span>
        <span></span>
      </div>
      *{List.mapi replay.parents ~f:row}
    </div>
  |}
;;

(* ---------- event log ---------- *)

(* The session narrated per order: arrivals, fills in plain words
   (bought/sold), completions with the final average, expiries. Built from
   the replay data at the current minute, newest first. *)
let event_log (replay : Replay.t) ~theme ~fills ~minute =
  let now = Replay.time_at replay ~minute in
  let events =
    List.concat_mapi replay.parents ~f:(fun index parent ->
      let instruction = parent.instruction in
      let side = instruction.Alpha_instruction.side in
      let total = Size.to_int instruction.Alpha_instruction.quantity in
      let mine =
        List.filter fills ~f:(fun (fill : Fill.t) ->
          Set.mem parent.order_ids fill.order_id)
      in
      let filled =
        List.sum (module Int) mine ~f:(fun fill -> Size.to_int fill.size)
      in
      let arrival =
        if Time_ns.Ofday.( <= )
             instruction.Alpha_instruction.arrival_time
             now
        then
          [ ( instruction.Alpha_instruction.arrival_time
            , 0
            , index
            , sprintf
                "%s %s %s arrives · deadline %s"
                (side_str side)
                (Symbol.to_string parent.symbol)
                (Int.to_string_hum ~delimiter:',' total)
                (hhmm instruction.Alpha_instruction.deadline)
            , None )
          ]
        else []
      in
      let fill_events =
        List.map mine ~f:(fun (fill : Fill.t) ->
          ( fill.time
          , 1
          , index
          , sprintf
              "%s %d @ %s · %s"
              (match fill.side with Buy -> "bought" | Sell -> "sold")
              (Size.to_int fill.size)
              (Price.to_string_dollar fill.price)
              (match fill.liquidity with
               | Taker -> "taker"
               | Maker -> "maker")
          , None ))
      in
      let completion =
        let rec find cum notional = function
          | [] -> []
          | (fill : Fill.t) :: rest ->
            let cum = cum + Size.to_int fill.size in
            let notional = notional + Fill.notional_cents fill in
            if cum >= total
            then
              [ ( fill.time
                , 2
                , index
                , sprintf
                    "complete · %s filled · avg $%.2f"
                    (Int.to_string_hum ~delimiter:',' cum)
                    (notional // cum /. 100.)
                , Some theme.Styles.green )
              ]
            else find cum notional rest
        in
        find 0 0 mine
      in
      let expiry =
        if Time_ns.Ofday.( > ) now instruction.Alpha_instruction.deadline
           && filled < total
        then
          [ ( instruction.Alpha_instruction.deadline
            , 3
            , index
            , sprintf
                "expired · %s unfilled"
                (Int.to_string_hum ~delimiter:',' (total - filled))
            , Some theme.Styles.red )
          ]
        else []
      in
      arrival @ fill_events @ completion @ expiry)
    |> List.sort
         ~compare:
           (fun
             ((t1 : Time_ns.Ofday.t), (p1 : int), (_ : int), (_ : string), _)
             (t2, p2, (_ : int), (_ : string), _)
           ->
           match Time_ns.Ofday.compare t1 t2 with
           | 0 -> Int.compare p1 p2
           | c -> c)
  in
  let row_view (time, (_ : int), index, text, color_override) =
    let style =
      Styles.s
        ("display:grid;grid-template-columns:64px 84px \
          1fr;column-gap:10px;padding:2px 16px;font-size:12.5px;color:"
         ^ Option.value color_override ~default:theme.Styles.secondary
         ^ ";"
         ^ Styles.mono)
    in
    let time_style = Styles.s ("color:" ^ theme.Styles.faint ^ ";") in
    let order_style =
      Styles.s
        ("color:" ^ Styles.order_color theme index ^ ";font-weight:600;")
    in
    {%html|
      <div %{style}>
        <span %{time_style}>#{hhmm time}</span>
        <span %{order_style}>Order %{index + 1#Int}</span>
        <span>#{text}</span>
      </div>
    |}
  in
  let title_style =
    Styles.s
      ("color:" ^ theme.Styles.text ^ ";font-size:14px;font-weight:600;")
  in
  let count_style =
    Styles.s
      ("color:" ^ theme.Styles.faint ^ ";font-size:13px;font-weight:400;")
  in
  let scroll =
    Styles.s
      "max-height:430px;overflow-y:auto;scrollbar-width:thin;display:flex;flex-direction:column-reverse;padding:8px \
       0;"
  in
  let header =
    Styles.s
      ("padding:14px 16px 8px 16px;border-bottom:1px solid "
       ^ theme.Styles.hairline
       ^ ";")
  in
  let count = sprintf " · %d events · newest first" (List.length events) in
  {%html|
    <div %{Styles.card theme ""}>
      <div %{header}>
        <span %{title_style}>Event log</span>
        <span %{count_style}>#{count}</span>
      </div>
      <div %{scroll}>*{List.map events ~f:row_view}</div>
    </div>
  |}
;;

(* ---------- screens ---------- *)

(* Buttons follow the mockup: compact rectangles with a 3px radius. Primary
   is the royal blue; secondary is a bordered paper button. *)
let primary_button ?(enabled = true) ?icon ~theme ~on_click label =
  let style =
    Styles.s
      ("display:inline-flex;align-items:center;gap:8px;background:"
       ^ theme.Styles.blue
       ^ ";color:#ffffff;border:1px solid "
       ^ theme.Styles.blue
       ^ ";border-radius:3px;padding:9px \
          18px;cursor:pointer;font-size:13.5px;font-weight:700;align-self:flex-start;white-space:nowrap;"
      )
  in
  let disabled = if enabled then Vdom.Attr.empty else Vdom.Attr.disabled in
  let glyph = match icon with None -> [] | Some icon -> [ icon ] in
  {%html|
    <button class="btn" %{style} %{disabled} on_click=%{on_click}>
      *{glyph}
      #{label}
    </button>
  |}
;;

let secondary_button ?icon ~theme ~on_click label =
  let style =
    Styles.s
      ("display:inline-flex;align-items:center;gap:8px;background:"
       ^ theme.Styles.card_bg
       ^ ";color:"
       ^ theme.Styles.text
       ^ ";border:1px solid "
       ^ theme.Styles.chip_border
       ^ ";border-radius:3px;padding:8px \
          16px;cursor:pointer;font-size:13px;font-weight:600;white-space:nowrap;"
      )
  in
  let glyph = match icon with None -> [] | Some icon -> [ icon ] in
  {%html|
    <button class="btn" %{style} on_click=%{on_click}>
      *{glyph}
      #{label}
    </button>
  |}
;;

(* The wizard's fixed navigation bar: Back bottom-left, Continue
   bottom-right, in the same place on every step. Sticky so the actions stay
   reachable on long screens. *)
let nav_footer ?back ?next ?status ~theme () =
  let bar =
    Styles.s
      ("position:sticky;bottom:12px;display:flex;justify-content:space-between;align-items:center;gap:12px;background:"
       ^ theme.Styles.card_bg
       ^ ";border:"
       ^ theme.Styles.border
       ^ ";border-radius:4px;padding:10px 14px;"
       ^ theme.Styles.shadow)
  in
  let back_node =
    match back with
    | Some (label, effect) ->
      secondary_button
        ~icon:(Icon.arrow_left ~size:14 ())
        ~theme
        ~on_click:(fun _ -> effect)
        label
    | None -> {%html|<span></span>|}
  in
  (* The mockup's quiet center readout: "AAPL · 2026-07-09 selected". *)
  let status_node =
    match status with
    | None -> {%html|<span></span>|}
    | Some text ->
      let style =
        Styles.s
          ("color:" ^ theme.Styles.faint ^ ";font-size:12px;" ^ Styles.mono)
      in
      {%html|<span %{style}>#{text}</span>|}
  in
  let next_node =
    match next with
    | Some (label, effect, enabled) ->
      primary_button
        ~enabled
        ~icon:(Icon.arrow_right ~size:14 ())
        ~theme
        ~on_click:(fun _ -> effect)
        label
    | None -> {%html|<span></span>|}
  in
  {%html|
    <div %{bar}>
      %{back_node}
      %{status_node}
      %{next_node}
    </div>
  |}
;;

let sim_view
  (replay : Replay.t)
  ~theme
  ~is_dark
  ~focus
  ~set_focus
  ~minute
  ~playing
  ~speed
  ~show_fills
  ~chart_view
  ~set_chart_view
  ~zoom_mode
  ~set_zoom_mode
  ~zoom_tool
  ~set_zoom_tool
  ~drag
  ~set_drag
  ~set_playing
  ~set_speed
  ~set_minute
  ~restart
  ~toggle_fills
  ~toggle_theme
  ~to_results
  ~back
  ~profile
  ~on_brand
  ~hover
  ~set_hover
  =
  let fills = Replay.fills_upto replay ~minute in
  (* The visible bar window: full session, or a preset span centered on the
     playhead (clamped at the edges), so the zoom follows the replay. *)
  let n = Replay.last_minute replay + 1 in
  let view =
    match (chart_view : Chart_view.t) with
    | Follow None -> 0, n - 1
    | Follow (Some span) ->
      (* A preset window rides the playhead, clamped at the session edges. *)
      clamp_window ~n ~z0:(minute - (span / 2)) ~span
    | Manual { z0; z1 } -> clamp_window ~n ~z0 ~span:(z1 - z0)
  in
  let page =
    Styles.s
      "display:flex;flex-direction:column;gap:20px;max-width:1320px;margin:0 \
       auto;padding:28px 20px;"
  in
  let title_style =
    Styles.s
      ("color:"
       ^ theme.Styles.text
       ^ ";font-size:21px;font-weight:700;margin:12px 0 4px;"
       ^ Styles.serif)
  in
  let sub_style =
    Styles.s
      ("color:" ^ theme.Styles.secondary ^ ";font-size:13px;line-height:1.7;")
  in
  let head_row =
    Styles.s
      ("display:flex;align-items:center;justify-content:space-between;gap:12px;flex-wrap:wrap;padding-bottom:12px;border-bottom:2px \
        solid "
       ^ theme.Styles.text
       ^ ";")
  in
  let title =
    sprintf
      "%s · %s · %s"
      (String.concat ~sep:" " (List.map replay.symbols ~f:Symbol.to_string))
      (Date.to_string replay.date)
      (String.uppercase replay.algo_name)
  in
  (* The runner reads the symbols out of the alpha, so the command line is
     the same however many names the file names. *)
  let command =
    (* Reproduces this run from the CLI, engine included; the fill-model
       dials are the CLI defaults, so non-default dials are labelled. *)
    sprintf
      "dune exec bin/main.exe -- <your_alpha.csv> %s %s%s"
      (Date.to_string replay.date)
      replay.algo_name
      (match replay.params.Execlab_session.Params.engine with
       | Execlab_session.Engine_choice.Bar_model -> ""
       | Synthetic { seed } -> sprintf " synthetic:%d" seed)
  in
  {%html|
    <div class="page fade" %{page}>
      <div>
        <div %{head_row}>
          %{wordmark ~on_click:on_brand ~theme ()}
          <span %{Styles.s "display:flex;gap:12px;align-items:center;"}>
            ?{profile}
            %{theme_button ~theme ~is_dark ~toggle_theme}
          </span>
        </div>
        %{step_progress ~theme ~current:3}
        <div %{title_style}>#{title}</div>
        <div %{sub_style}>
          source: <span %{Styles.code_chip theme}>#{command}</span>
        </div>
      </div>
      %{controls replay ~theme ~minute ~playing ~speed ~chart_view
          ~set_chart_view ~zoom_mode ~set_zoom_mode ~zoom_tool
          ~set_zoom_tool ~set_playing
          ~set_speed ~set_minute ~restart}
      <div %{Styles.card theme "padding-bottom:8px;"}>
        ?{symbol_tabs replay ~theme ~focus ~set_focus}
        %{legend replay ~theme ~focus ~minute ~fills ~show_fills
            ~toggle_fills}
        %{chart replay ~theme ~focus ~minute ~fills ~show_fills ~hover
            ~set_hover ~view ~zoom_mode ~zoom_tool ~drag ~set_drag
            ~set_view:(fun (z0, z1) ->
              set_chart_view (Chart_view.Manual { z0; z1 }))}
      </div>
      <div %{Styles.s "overflow-x:auto;min-width:0;"}>
        %{orders_table replay ~theme ~fills ~minute}
      </div>
      %{event_log replay ~theme ~fills ~minute}
      %{nav_footer ~theme
          ~back:("New simulation", back)
          ~next:("Results", to_results, true) ()}
    </div>
  |}
;;

(* The parsed-alpha table shared by the alpha and setup screens: mono rows
   under small-caps column heads, sides colored the mockup's way — buys blue,
   sells red. *)
let instructions_columns = "52px 76px 64px 1fr"

let instructions_header ~theme =
  {%html|
    <div
      %{Styles.s
          ("display:grid;grid-template-columns:"
           ^ instructions_columns
           ^ ";column-gap:14px;padding:8px 0 6px;border-bottom:1px solid "
           ^ theme.Styles.hairline
           ^ ";"
           ^ Styles.table_label theme)}>
      <span>side</span>
      <span>qty</span>
      <span>sym</span>
      <span>arrival → deadline</span>
    </div>
  |}
;;

let instruction_row ~theme (instruction : Alpha_instruction.t) =
  let row =
    Styles.s
      ("display:grid;grid-template-columns:"
       ^ instructions_columns
       ^ ";column-gap:14px;padding:7px 0;border-bottom:1px solid "
       ^ theme.Styles.hairline
       ^ ";font-size:12.5px;color:"
       ^ theme.Styles.text
       ^ ";"
       ^ Styles.mono)
  in
  let side_color =
    match instruction.Alpha_instruction.side with
    | Buy -> theme.Styles.blue
    | Sell -> theme.Styles.red
  in
  let side_style = Styles.s ("font-weight:700;color:" ^ side_color ^ ";") in
  let dim = Styles.s ("color:" ^ theme.Styles.secondary ^ ";") in
  let window =
    sprintf
      "%s → %s"
      (hhmm instruction.arrival_time)
      (hhmm instruction.deadline)
  in
  {%html|
    <div %{row}>
      <span %{side_style}>#{side_str instruction.side}</span>
      <span>#{Int.to_string_hum ~delimiter:',' (Size.to_int instruction.quantity)}</span>
      <span>#{Symbol.to_string instruction.symbol}</span>
      <span %{dim}>#{window}</span>
    </div>
  |}
;;

(* The wizard's five stations, in order; [step_progress] renders where the
   user is, with completed stations checked off. *)
(* Shared page chrome for the wizard screens: brand, title, optional back
   link, theme toggle, and (when [step] is given) the progress rail. *)
let wizard_header
  ?step
  ?profile
  ?kicker
  ?on_brand
  ~theme
  ~is_dark
  ~toggle_theme
  ~title
  ~subtitle
  ~back
  ()
  =
  (* Mockup chrome: a rule-bounded top bar carrying the wordmark and the
     header actions, then a serif title block, then the numbered rail. *)
  let bar =
    Styles.s
      ("display:flex;justify-content:space-between;align-items:center;gap:12px;flex-wrap:wrap;padding:2px \
        0 12px;border-bottom:2px solid "
       ^ theme.Styles.text
       ^ ";")
  in
  let title_style =
    Styles.s
      ("color:"
       ^ theme.Styles.text
       ^ ";font-size:27px;font-weight:700;margin:14px 0 \
          4px;letter-spacing:-0.01em;"
       ^ Styles.serif)
  in
  let sub_style =
    Styles.s ("color:" ^ theme.Styles.secondary ^ ";font-size:13.5px;")
  in
  let kicker_node =
    match kicker with
    | None -> []
    | Some text ->
      [ {%html|<div %{Styles.s (Styles.kicker theme ^ "margin-top:16px;")}>#{text}</div>|}
      ]
  in
  let back_style =
    Styles.s
      ("background:none;border:none;color:"
       ^ theme.Styles.blue
       ^ ";cursor:pointer;font-size:12.5px;font-weight:600;padding:6px 8px;"
      )
  in
  let back_button =
    match back with
    | None -> []
    | Some (label, effect) ->
      [ {%html|<button class="btn" %{back_style} on_click=%{fun _ -> effect}>#{label}</button>|}
      ]
  in
  let progress =
    match step with
    | None -> []
    | Some current -> [ step_progress ~theme ~current ]
  in
  {%html|
    <div>
      <div %{bar}>
        %{wordmark ?on_click:on_brand ~theme ()}
        <span %{Styles.s "display:flex;gap:12px;align-items:center;"}>
          *{back_button}
          ?{profile}
          %{theme_button ~theme ~is_dark ~toggle_theme}
        </span>
      </div>
      *{progress}
      *{kicker_node}
      <div %{title_style}>#{title}</div>
      <div %{sub_style}>#{subtitle}</div>
    </div>
  |}
;;

let narrow_page =
  "display:flex;flex-direction:column;gap:20px;max-width:1320px;margin:32px \
   auto;padding:20px;"
;;

(* Side-by-side halves for the wizard screens; collapses on narrow windows. *)
let two_col =
  "display:grid;grid-template-columns:repeat(auto-fit,minmax(420px,1fr));gap:16px;align-items:start;"
;;

let run_record (replay : Replay.t) =
  let rows = replay.results.rows in
  let sum f = List.sum (module Int) rows ~f in
  let net =
    sum (fun row -> row.Replay.grading.Transaction_cost.net_pnl_cents)
  in
  let gross =
    sum (fun row -> row.Replay.grading.gross_theoretical_pnl_cents)
  in
  let filled = sum (fun row -> Size.to_int row.Replay.grading.filled) in
  let ordered = sum (fun row -> Size.to_int row.Replay.grading.quantity) in
  (* One shortfall number for the whole run: each order's bps weighted by the
     shares that actually traded, since an unfilled order has no execution to
     grade. *)
  let shortfall_bps =
    let weighted =
      List.sum (module Float) rows ~f:(fun row ->
        match row.Replay.grading.fill_metrics with
        | None -> 0.
        | Some metrics ->
          metrics.Transaction_cost.Fill_metrics.shortfall_bps
          *. Float.of_int (Size.to_int row.Replay.grading.filled))
    in
    if filled > 0 then weighted /. Float.of_int filled else 0.
  in
  let fill = replay.params.Execlab_session.Params.fill_config in
  let ran_at =
    (* A display label and a newest-first sort key; browser wall clock. *)
    let date = new%js Js_of_ocaml.Js.date_now in
    Js_of_ocaml.Js.to_string date##toISOString
  in
  { History.Run_record.symbols = replay.symbols
  ; date = replay.date
  ; algo_name = replay.algo_name
  ; alpha_capture = (if gross > 0 then Some (net // gross) else None)
  ; value_add_cents = replay.results.total_value_add_cents
  ; net_cents = net
  ; shortfall_bps
  ; completion =
      (if ordered > 0
       then Float.of_int filled /. Float.of_int ordered
       else 0.)
  ; alpha_text = replay.alpha_text
  ; half_spread_cents = Price.to_int_cents fill.half_spread
  ; max_participation = fill.max_participation
  ; impact_coefficient_cents = Price.to_int_cents fill.impact_coefficient
  ; pov_rate = replay.params.pov_rate
  ; is_urgency = replay.params.is_urgency
  ; patience = replay.params.patience
  ; engine_name =
      (match replay.params.engine with
       | Execlab_session.Engine_choice.Bar_model -> "bar"
       | Synthetic { seed = (_ : int) } -> "synthetic")
  ; engine_seed =
      (match replay.params.engine with
       | Execlab_session.Engine_choice.Bar_model -> 0
       | Synthetic { seed } -> seed)
  ; ran_at
  }
;;

(* Rebuilds the exact simulation a history row records — the local
   counterpart of what reopening a server run used to do. *)
let replay_of_record (record : History.Run_record.t) =
  Replay.run
    ~date:record.date
    ~alpha_text:record.alpha_text
    ~algo_name:record.algo_name
    ~params:
      { Execlab_session.Params.fill_config =
          { half_spread = Price.of_int_cents record.half_spread_cents
          ; max_participation = record.max_participation
          ; impact_coefficient =
              Price.of_int_cents record.impact_coefficient_cents
          }
      ; pov_rate = record.pov_rate
      ; is_urgency = record.is_urgency
      ; patience = record.patience
      ; engine =
          (match record.engine_name with
           | "synthetic" ->
             Execlab_session.Engine_choice.Synthetic
               { seed = record.engine_seed }
           | (_ : string) -> Execlab_session.Engine_choice.Bar_model)
      }
;;

(* ---------- "what changed?" ---------- *)

(* A single before/after readout. [better] decides the color and arrow, so
   each metric declares its own direction: capture and completion are better
   when they rise, shortfall when it falls. *)
let delta_tile ~theme ~label ~value ~delta ~better ~unit_ =
  let color =
    match better with
    | `Better -> theme.Styles.green
    | `Worse -> theme.Styles.red
    | `Same -> theme.Styles.faint
  in
  let arrow =
    match better with
    | `Better -> "\u{2191}"
    | `Worse -> "\u{2193}"
    | `Same -> "\u{2192}"
  in
  let tile =
    Styles.s
      ("display:flex;flex-direction:column;gap:3px;background:"
       ^ theme.Styles.chip_bg
       ^ ";border:1px solid "
       ^ theme.Styles.chip_border
       ^ ";border-radius:3px;padding:14px 16px;")
  in
  let value_style =
    Styles.s
      ("color:"
       ^ theme.Styles.text
       ^ ";font-size:24px;font-weight:800;"
       ^ Styles.mono)
  in
  let delta_style =
    Styles.s
      ("color:" ^ color ^ ";font-size:13px;font-weight:700;" ^ Styles.mono)
  in
  {%html|
    <div %{tile}>
      <span %{Styles.s (Styles.label theme)}>#{label}</span>
      <span %{value_style}>#{value}</span>
      <span %{delta_style}>#{arrow} #{delta} #{unit_}</span>
    </div>
  |}
;;

let what_changed ~theme ~(current : History.Run_record.t) ~previous =
  match (previous : History.Run_record.t option) with
  | None -> []
  | Some previous ->
    let direction ~higher_is_better ~now ~before =
      let epsilon = 1e-9 in
      if Float.( < ) (Float.abs (now -. before)) epsilon
      then `Same
      else if Bool.equal (Float.( > ) now before) higher_is_better
      then `Better
      else `Worse
    in
    let capture_of (record : History.Run_record.t) =
      Option.value record.alpha_capture ~default:0. *. 100.
    in
    let capture_now = capture_of current
    and capture_before = capture_of previous in
    let completion_now = current.completion *. 100.
    and completion_before = previous.completion *. 100. in
    let grid =
      Styles.s
        "display:grid;grid-template-columns:repeat(auto-fit,minmax(190px,1fr));gap:10px;"
    in
    let heading =
      Styles.s
        ("color:"
         ^ theme.Styles.text
         ^ ";font-size:15px;font-weight:700;margin-bottom:3px;")
    in
    let sub =
      Styles.s
        ("color:"
         ^ theme.Styles.faint
         ^ ";font-size:12.5px;margin-bottom:14px;")
    in
    [ {%html|
        <div %{Styles.card theme "padding:20px;"}>
          <div %{heading}>What changed?</div>
          <div %{sub}>
            #{sprintf "versus your previous run — %s %s, %s"
                (History.Run_record.symbols_label previous)
                (Date.to_string previous.date)
                (String.uppercase previous.algo_name)}
          </div>
          <div %{grid}>
            %{delta_tile ~theme ~label:"Alpha captured"
                ~value:(sprintf "%.1f%%" capture_now)
                ~delta:(sprintf "%+.1f" (capture_now -. capture_before))
                ~better:(direction ~higher_is_better:true ~now:capture_now
                           ~before:capture_before)
                ~unit_:"pts"}
            %{delta_tile ~theme ~label:"Implementation shortfall"
                ~value:(sprintf "%.1f bp" current.shortfall_bps)
                ~delta:(sprintf "%+.1f"
                          (current.shortfall_bps -. previous.shortfall_bps))
                ~better:(direction ~higher_is_better:false
                           ~now:current.shortfall_bps
                           ~before:previous.shortfall_bps)
                ~unit_:"bp"}
            %{delta_tile ~theme ~label:"Completion"
                ~value:(sprintf "%.0f%%" completion_now)
                ~delta:(sprintf "%+.0f" (completion_now -. completion_before))
                ~better:(direction ~higher_is_better:true ~now:completion_now
                           ~before:completion_before)
                ~unit_:"pts"}
            %{delta_tile ~theme ~label:"Execution bonus"
                ~value:(dollars_signed current.value_add_cents)
                ~delta:(dollars_signed
                          (current.value_add_cents - previous.value_add_cents))
                ~better:(direction ~higher_is_better:true
                           ~now:(Float.of_int current.value_add_cents)
                           ~before:(Float.of_int previous.value_add_cents))
                ~unit_:""}
          </div>
        </div>
      |}
    ]
;;

(* ---------- my runs: the execution notebook ---------- *)

(* Cost convention: positive = money lost, so red; negative = favorable. *)
let cost_cell ~theme cents =
  let color =
    if cents > 0
    then theme.Styles.red
    else if cents < 0
    then theme.Styles.green
    else theme.Styles.faint
  in
  let style =
    Styles.s ("color:" ^ color ^ ";font-size:13px;" ^ Styles.mono)
  in
  {%html|<span %{style}>#{dollars_signed cents}</span>|}
;;

(* Board totals arrive as Int63 (32-bit browser ints would overflow on large
   positions), so they format through floats. *)
(* P&L convention: positive = money made, so green. *)
let pnl_cell ~theme cents =
  let color =
    if cents > 0
    then theme.Styles.green
    else if cents < 0
    then theme.Styles.red
    else theme.Styles.faint
  in
  let style =
    Styles.s
      ("color:" ^ color ^ ";font-size:13px;font-weight:600;" ^ Styles.mono)
  in
  {%html|<span %{style}>#{dollars_signed cents}</span>|}
;;

let my_runs_view
  ~theme
  ~is_dark
  ~profile
  ~on_brand
  ~toggle_theme
  ~runs
  ~run_error
  ~open_run
  ~rerun
  ~set_baseline
  ~delete
  ~clear_all
  ~confirm_clear
  ~new_sim
  ~back
  =
  let columns = "104px 88px 92px 66px 1fr 118px 96px 70px 260px" in
  let head =
    Styles.s
      ("display:grid;grid-template-columns:"
       ^ columns
       ^ ";column-gap:12px;white-space:nowrap;padding:10px 16px \
          8px;border-bottom:1px solid "
       ^ theme.Styles.hairline
       ^ ";"
       ^ Styles.table_label theme)
  in
  let dim = Styles.s ("color:" ^ theme.Styles.secondary ^ ";") in
  let faint_style =
    Styles.s ("color:" ^ theme.Styles.faint ^ ";font-size:12px;")
  in
  let error_banner =
    match (run_error : Error.t option) with
    | None -> []
    | Some error ->
      [ {%html|
          <div
            %{Styles.s
                ("border:1px solid "
                 ^ theme.Styles.red
                 ^ ";border-radius:3px;padding:10px 14px;color:"
                 ^ theme.Styles.red
                 ^ ";font-size:12.5px;"
                 ^ Styles.mono)}>
            #{Error.to_string_hum error}
          </div>
        |}
      ]
  in
  let params_of (run : History.Run_record.t) =
    match run.algo_name with
    | "pov" -> sprintf "rate %.4f" run.pov_rate
    | "is" -> sprintf "urgency %.1f" run.is_urgency
    | "adaptive" -> sprintf "patience %.2f" run.patience
    | (_ : string) -> "—"
  in
  let full_params (run : History.Run_record.t) =
    sprintf
      "half spread $%.2f · participation %.2f · impact $%.2f · engine %s"
      (Float.of_int run.half_spread_cents /. 100.)
      run.max_participation
      (Float.of_int run.impact_coefficient_cents /. 100.)
      run.engine_name
  in
  let small_action ?(danger = false) ~label ~on_click () =
    let style =
      Styles.s
        ("background:"
         ^ theme.Styles.chip_bg
         ^ ";color:"
         ^ (if danger then theme.Styles.red else theme.Styles.text)
         ^ ";border:1px solid "
         ^ theme.Styles.chip_border
         ^ ";border-radius:5px;padding:4px \
            9px;cursor:pointer;font-size:11px;font-weight:700;")
    in
    {%html|<button class="btn" %{style} on_click=%{on_click}>#{label}</button>|}
  in
  let run_row (run : History.Run_record.t) =
    let style =
      Styles.s
        ("display:grid;grid-template-columns:"
         ^ columns
         ^ ";column-gap:12px;white-space:nowrap;align-items:center;padding:8px \
            16px;font-size:12.5px;color:"
         ^ theme.Styles.text
         ^ ";border-bottom:1px solid "
         ^ theme.Styles.hairline
         ^ ";"
         ^ Styles.mono)
    in
    let capture =
      match run.alpha_capture with
      | None -> "—"
      | Some capture -> sprintf "%.1f%%" (capture *. 100.)
    in
    {%html|
      <div %{style} title=%{full_params run}>
        <span %{dim}>#{String.prefix run.ran_at 10}</span>
        <span>#{History.Run_record.symbols_label run}</span>
        <span %{dim}>#{Date.to_string run.date}</span>
        <span>#{String.uppercase run.algo_name}</span>
        <span %{dim}>#{params_of run}</span>
        <span>%{pnl_cell ~theme run.value_add_cents}</span>
        <span>#{sprintf "%+.1f bps" run.shortfall_bps}</span>
        <span %{dim}>#{capture}</span>
        <span %{Styles.s "display:flex;gap:6px;"}>
          %{small_action ~label:"Open"
              ~on_click:(fun _ -> open_run run) ()}
          %{small_action ~label:"Rerun"
              ~on_click:(fun _ -> rerun run) ()}
          %{small_action ~label:"Baseline"
              ~on_click:(fun _ -> set_baseline run) ()}
          %{small_action ~danger:true ~label:"Delete"
              ~on_click:(fun _ -> delete run) ()}
        </span>
      </div>
    |}
  in
  let body =
    match (runs : History.Run_record.t list) with
    | [] ->
      [ {%html|
          <div
            %{Styles.s
                "padding:26px 16px;display:flex;flex-direction:column;gap:12px;align-items:flex-start;"}>
            <span %{faint_style}>
              No runs yet. Every simulation you execute is saved here, on
              this machine.
            </span>
            %{primary_button ~icon:(Icon.arrow_right ~size:15 ()) ~theme
                ~on_click:(fun _ -> new_sim) "Run your first simulation"}
          </div>
        |}
      ]
    | runs ->
      {%html|
        <div %{head}>
          <span>ran</span>
          <span>symbols</span>
          <span>day</span>
          <span>algo</span>
          <span>settings</span>
          <span>vs immediate</span>
          <span>slippage</span>
          <span>capture</span>
          <span>actions</span>
        </div>
      |}
      :: List.map runs ~f:run_row
  in
  (* Destructive, so it asks twice. *)
  let clear_button =
    let style =
      Styles.s
        ("display:inline-flex;align-items:center;gap:6px;background:"
         ^ (if confirm_clear then theme.Styles.red else theme.Styles.chip_bg)
         ^ ";color:"
         ^ (if confirm_clear then "#ffffff" else theme.Styles.red)
         ^ ";border:1px solid "
         ^ (if confirm_clear
            then theme.Styles.red
            else theme.Styles.chip_border)
         ^ ";border-radius:7px;padding:6px \
            11px;cursor:pointer;font-size:12px;font-weight:700;")
    in
    {%html|
      <button class="btn" %{style} on_click=%{fun _ -> clear_all}>
        #{if confirm_clear
          then "Click again to erase all runs"
          else "Clear history"}
      </button>
    |}
  in
  {%html|
    <div class="page fade" %{Styles.s narrow_page}>
      %{wizard_header ?profile ?on_brand:(Some on_brand) ~theme ~is_dark
          ~toggle_theme ~title:"My runs"
          ~subtitle:"every simulation you've executed, stored on this \
                     machine"
          ~back:(Some ("← Dashboard", back)) ()}
      *{error_banner}
      <div %{Styles.card theme "padding-bottom:4px;"}>
        <div
          %{Styles.s
              "display:flex;justify-content:space-between;align-items:center;padding:14px 16px 4px 16px;"}>
          <span
            %{Styles.s
                ("color:"
                 ^ theme.Styles.text
                 ^ ";font-size:14.5px;font-weight:700;")}>
            Saved runs
          </span>
          %{clear_button}
        </div>
        <div %{Styles.s "overflow-x:auto;"}>
          <div %{Styles.s "min-width:980px;"}>
            *{body}
          </div>
        </div>
        <div
          %{Styles.s
              ("color:"
               ^ theme.Styles.faint
               ^ ";font-size:11.5px;padding:10px 16px 12px;line-height:1.5;")}>
          Open restores a run's exact results. Rerun starts a new setup from
          its day and instructions. Baseline makes it the run your next
          results compare against.
        </div>
      </div>
      %{nav_footer ~theme
          ~back:("Dashboard", back)
          ~next:("New simulation", new_sim, true) ()}
    </div>
  |}
;;

(* ---------- metrics glossary ---------- *)

(* Plain-English definitions for every number on the results screen, shown in
   a modal behind the (?) button. Kept as data so the copy stays reviewable
   in one place. *)
let glossary_intro =
  "This screen compares two worlds: the profit your trade ideas would have \
   made in a perfect, costless market, and what was actually left after \
   trading them in a real historical session. For anything labelled a cost, \
   a positive number means money lost and a negative number means the \
   market moved in your favor; for profit figures, positive is simply \
   profit."
;;

let glossary_entries =
  [ ( "Your alpha predicted / gross alpha"
    , "The profit your trade ideas would have made if trading were instant \
       and free."
    , "The full ordered quantity valued from the arrival price to the \
       session's close, with zero costs. It is the ceiling everything else \
       is measured against." )
  ; ( "You actually kept / net P&L"
    , "What the trades really earned after every cost of executing them."
    , "Gross alpha minus shortfall minus opportunity cost. A big gap below \
       \"Your alpha predicted\" means execution ate the idea." )
  ; ( "Alpha captured / capture"
    , "The share of the predicted profit that survived trading, as a \
       percentage."
    , "Net P&L divided by gross alpha. 100% is costless execution; under \
       50% means costs dominate. Blank when the prediction lost money, \
       since the ratio is then meaningless." )
  ; ( "Execution bonus / vs immediate"
    , "Extra dollars your algorithm made versus dumping every order the \
       instant it arrived."
    , "Your net P&L minus the Immediate baseline's, on the identical orders \
       and day. Positive means patience paid off; negative means trading it \
       all at once would have been better." )
  ; ( "= shortfall"
    , "The total cost of execution: what the filled shares cost beyond the \
       ideal price."
    , "Exactly timing + spread + impact, with no leftover. Positive means \
       money lost to execution; negative means the market moved your way. \
       Lower is better." )
  ; ( "timing"
    , "What the market's own price drift between your decision and your \
       fills cost you."
    , "Compares each fill's minute against the arrival price. Positive: \
       prices moved against you while you waited. Negative: the drift \
       helped." )
  ; ( "+ spread"
    , "The toll for demanding an immediate fill instead of waiting for \
       someone to meet you."
    , "Buyers bid a little below what sellers ask; taking a fill now costs \
       half that gap per share. Patient limit orders pay none of it." )
  ; ( "+ impact"
    , "How much your own buying or selling pushed the price against you."
    , "Each fill's distance beyond its minute's opening price plus the \
       spread toll. It grows with the share of that minute's volume you \
       take, so trading faster raises it." )
  ; ( "in bps"
    , "The same shortfall as a share of price, so orders of any size can be \
       compared."
    , "Basis points: 1 bp = 0.01% of the arrival price. Roughly, 5 bps is \
       cheap and 50 bps is expensive. Positive is always worse, on either \
       side." )
  ; ( "arrival price"
    , "The market price at the moment an order went live — the scorecard's \
       starting line."
    , "The opening price of the first minute at or after the instruction's \
       timestamp. Everything paid above it when buying, or received below \
       it when selling, becomes shortfall." )
  ; ( "avg fill"
    , "The average price you actually traded at, weighted by the size of \
       each fill."
    , "Compare it with the arrival price: for a buy, lower is better; for a \
       sell, higher is better." )
  ; ( "day VWAP"
    , "The average price the whole market traded at that day."
    , "Volume-weighted average price: every trade in the session weighted \
       by its size. Buying below it, or selling above it, beats the typical \
       participant." )
  ; ( "filled"
    , "The share of the ordered quantity that actually traded before the \
       deadline."
    , "Less than 100% means the algorithm ran out of time or of willing \
       counterparties, and the missing shares reappear as opportunity cost."
    )
  ; ( "opportunity"
    , "The profit lost on shares that never traded."
    , "Values the unfilled remainder from the arrival price to the session \
       close. Positive: those shares would have made money you missed. \
       Negative: a lucky escape, because the idea was wrong." )
  ]
;;

let help_modal ~theme ~close =
  let backdrop =
    Styles.s
      "position:fixed;inset:0;background:rgba(8,12,20,0.55);display:flex;align-items:flex-start;justify-content:center;padding:40px \
       20px;z-index:50;overflow:auto;"
  in
  let panel =
    Styles.s
      ("background:"
       ^ theme.Styles.card_bg
       ^ ";border:"
       ^ theme.Styles.border
       ^ ";border-radius:4px;max-width:760px;width:100%;padding:24px;"
       ^ theme.Styles.shadow)
  in
  let head =
    Styles.s
      "display:flex;justify-content:space-between;align-items:flex-start;gap:16px;margin-bottom:10px;"
  in
  let title_style =
    Styles.s
      ("color:" ^ theme.Styles.text ^ ";font-size:19px;font-weight:800;")
  in
  let intro_style =
    Styles.s
      ("color:"
       ^ theme.Styles.secondary
       ^ ";font-size:13px;line-height:1.65;margin-bottom:16px;")
  in
  let entry (term, plain, detail) =
    let row =
      Styles.s
        ("padding:11px 0;border-top:1px solid " ^ theme.Styles.hairline ^ ";")
    in
    let term_style =
      Styles.s
        ("color:"
         ^ theme.Styles.blue
         ^ ";font-size:13px;font-weight:700;margin-bottom:3px;"
         ^ Styles.mono)
    in
    let plain_style =
      Styles.s
        ("color:" ^ theme.Styles.text ^ ";font-size:13px;line-height:1.55;")
    in
    let detail_style =
      Styles.s
        ("color:"
         ^ theme.Styles.faint
         ^ ";font-size:12px;line-height:1.6;margin-top:2px;")
    in
    {%html|
      <div %{row}>
        <div %{term_style}>#{term}</div>
        <div %{plain_style}>#{plain}</div>
        <div %{detail_style}>#{detail}</div>
      </div>
    |}
  in
  {%html|
    <div %{backdrop} on_click=%{fun _ -> close}>
      <div
        class="fade"
        %{panel}
        on_click=%{fun (_ : _) -> Effect.Ignore}>
        <div %{head}>
          <span %{title_style}>Reading your results</span>
          %{secondary_button ~theme ~on_click:(fun _ -> close) "Close"}
        </div>
        <div %{intro_style}>#{glossary_intro}</div>
        <div
          %{Styles.s
              "display:grid;grid-template-columns:repeat(auto-fit,minmax(320px,1fr));column-gap:28px;"}>
          *{List.map glossary_entries ~f:entry}
        </div>
      </div>
    </div>
  |}
;;

let dashboard_view
  ~theme
  ~is_dark
  ~runs
  ~new_sim
  ~to_my_runs
  ~profile
  ~on_brand
  ~toggle_theme
  =
  let empty_style =
    Styles.s
      ("color:" ^ theme.Styles.faint ^ ";font-size:12.5px;padding:12px 16px;")
  in
  (* Money in table cells: colored by sign, mono, no inline label — the
     column header already names it. *)
  let money_cell cents =
    let color =
      if cents > 0
      then theme.Styles.green
      else if cents < 0
      then theme.Styles.red
      else theme.Styles.faint
    in
    let style =
      Styles.s
        ("color:"
         ^ color
         ^ ";font-size:12.5px;font-weight:600;"
         ^ Styles.mono)
    in
    {%html|<span %{style}>#{dollars_signed cents}</span>|}
  in
  let section_head ~title ~aside =
    let title_style =
      Styles.s
        ("color:" ^ theme.Styles.text ^ ";font-size:14.5px;font-weight:700;")
    in
    let aside_style =
      Styles.s
        ("color:" ^ theme.Styles.faint ^ ";font-size:10.5px;" ^ Styles.mono)
    in
    {%html|
      <div
        %{Styles.s
            "display:flex;justify-content:space-between;align-items:baseline;gap:12px;padding:14px 16px 10px;"}>
        <span %{title_style}>#{title}</span>
        <span %{aside_style}>#{aside}</span>
      </div>
    |}
  in
  let row_base =
    "display:grid;grid-template-columns:150px 80px 1fr 1fr \
     70px;column-gap:10px;align-items:baseline;"
  in
  let head_row =
    Styles.s
      (row_base
       ^ "padding:8px 16px 6px;border-bottom:1px solid "
       ^ theme.Styles.hairline
       ^ ";"
       ^ Styles.table_label theme)
  in
  let run_row (run : History.Run_record.t) =
    let style =
      Styles.s
        (row_base
         ^ "padding:8px 16px;font-size:12.5px;color:"
         ^ theme.Styles.text
         ^ ";border-bottom:1px solid "
         ^ theme.Styles.hairline
         ^ ";"
         ^ Styles.mono)
    in
    let dim = Styles.s ("color:" ^ theme.Styles.secondary ^ ";") in
    let capture =
      match run.alpha_capture with
      | None -> "—"
      | Some c -> sprintf "%.1f%%" (c *. 100.)
    in
    {%html|
      <div %{style}>
        <span>#{History.Run_record.symbols_label run} · #{Date.to_string run.date}</span>
        <span %{dim}>#{String.uppercase run.algo_name}</span>
        <span>%{money_cell run.value_add_cents}</span>
        <span>%{money_cell run.net_cents}</span>
        <span %{dim}>#{capture}</span>
      </div>
    |}
  in
  let table body =
    match body with
    | [] ->
      [ {%html|<div %{empty_style}>No runs yet — run your first simulation.</div>|}
      ]
    | rows ->
      {%html|
        <div %{head_row}>
          <span>day</span>
          <span>algo</span>
          <span>vs immediate</span>
          <span>net P&L</span>
          <span>capture</span>
        </div>
      |}
      :: List.map rows ~f:run_row
  in
  let best =
    runs
    |> List.sort
         ~compare:
           (Comparable.lift
              (Comparable.reverse Int.compare)
              ~f:(fun (run : History.Run_record.t) -> run.value_add_cents))
    |> fun sorted -> List.take sorted 5
  in
  (* The ranked sidebar: an amber rank, the run's identity, its value added. *)
  let best_list =
    match best with
    | [] -> [ {%html|<div %{empty_style}>Nothing ranked yet.</div>|} ]
    | rows ->
      List.mapi rows ~f:(fun index (run : History.Run_record.t) ->
        let row =
          Styles.s
            ("display:flex;gap:10px;align-items:baseline;padding:8px \
              16px;border-bottom:1px solid "
             ^ theme.Styles.hairline
             ^ ";font-size:12.5px;"
             ^ Styles.mono)
        in
        let rank =
          Styles.s
            ("color:" ^ theme.Styles.brown ^ ";font-weight:700;width:16px;")
        in
        let name = Styles.s ("color:" ^ theme.Styles.text ^ ";flex:1;") in
        {%html|
          <div %{row}>
            <span %{rank}>#{Int.to_string (index + 1)}</span>
            <span %{name}>
              #{History.Run_record.symbols_label run}
              · #{Date.to_string run.date}
              · #{String.uppercase run.algo_name}
            </span>
            <span>%{money_cell run.value_add_cents}</span>
          </div>
        |})
  in
  let best_note =
    Styles.s
      ("color:"
       ^ theme.Styles.faint
       ^ ";font-size:11.5px;line-height:1.6;padding:10px 16px 14px;")
  in
  (* The mockup's stats strip: one bordered band, hairline-divided cells,
     label over value. All four come straight from the local run history. *)
  let stats_band =
    let count = List.length runs in
    let best_value =
      List.map runs ~f:(fun (run : History.Run_record.t) ->
        run.value_add_cents)
      |> List.max_elt ~compare:Int.compare
    in
    let best_capture =
      List.filter_map runs ~f:(fun (run : History.Run_record.t) ->
        run.alpha_capture)
      |> List.max_elt ~compare:Float.compare
    in
    let lowest_shortfall =
      List.map runs ~f:(fun (run : History.Run_record.t) ->
        run.shortfall_bps)
      |> List.min_elt ~compare:Float.compare
    in
    let most_used_algo =
      List.map runs ~f:(fun (run : History.Run_record.t) -> run.algo_name)
      |> List.sort_and_group ~compare:String.compare
      |> List.max_elt ~compare:(Comparable.lift Int.compare ~f:List.length)
      |> Option.bind ~f:List.hd
    in
    let cell index ~label:label_text ~color value =
      let style =
        Styles.s
          ("display:flex;flex-direction:column;gap:6px;padding:14px 18px;"
           ^
           if index = 0
           then ""
           else "border-left:1px solid " ^ theme.Styles.hairline ^ ";")
      in
      let value_style =
        Styles.s
          ("color:"
           ^ color
           ^ ";font-size:20px;font-weight:700;white-space:nowrap;"
           ^ Styles.mono)
      in
      {%html|
        <div %{style}>
          <span %{Styles.s (Styles.label theme)}>#{label_text}</span>
          <span %{value_style}>#{value}</span>
        </div>
      |}
    in
    let best_color =
      match best_value with
      | Some v when v > 0 -> theme.Styles.green
      | Some v when v < 0 -> theme.Styles.red
      | Some (_ : int) | None -> theme.Styles.faint
    in
    {%html|
      <div %{Styles.card theme ""}>
        <div
          %{Styles.s
              "display:grid;grid-template-columns:repeat(auto-fit,minmax(200px,1fr));"}>
          %{cell 0 ~label:"total simulations" ~color:theme.Styles.text
              (Int.to_string count)}
          %{cell 1 ~label:"best alpha capture" ~color:best_color
              (match best_capture with
               | None -> "—"
               | Some c -> sprintf "%.1f%%" (c *. 100.))}
          %{cell 2 ~label:"lowest slippage" ~color:theme.Styles.text
              (match lowest_shortfall with
               | None -> "—"
               | Some bps -> sprintf "%+.1f bps" bps)}
          %{cell 3 ~label:"most used algorithm"
              ~color:theme.Styles.secondary
              (match most_used_algo with
               | None -> "—"
               | Some algo -> String.uppercase algo)}
        </div>
      </div>
    |}
  in
  {%html|
    <div class="page fade" %{Styles.s narrow_page}>
      %{wizard_header ?profile ~on_brand ~theme ~is_dark ~toggle_theme
          ~title:"Historical execution laboratory"
          ~subtitle:"upload an alpha, pick a day, and see how much survives \
                     execution"
          ~back:None ()}
      <div %{Styles.s "display:flex;gap:10px;flex-wrap:wrap;align-items:center;"}>
        %{primary_button ~icon:(Icon.arrow_right ~size:15 ()) ~theme
            ~on_click:(fun _ -> new_sim) "New simulation"}
        %{secondary_button ~theme ~on_click:(fun _ -> to_my_runs)
            "My runs"}
      </div>
      %{stats_band}
      <div %{Styles.s two_col}>
        <div %{Styles.card theme "padding-bottom:4px;"}>
          %{section_head ~title:"Recent runs" ~aside:"newest first"}
          *{table runs}
        </div>
        <div %{Styles.card theme "padding-bottom:4px;"}>
          %{section_head ~title:"Best runs"
              ~aside:"by value added vs Immediate"}
          *{best_list}
          <div %{best_note}>
            Value added is net P&L minus what an Immediate execution of the
            same instructions would have kept — the cleanest read on
            execution skill.
          </div>
        </div>
      </div>
    </div>
  |}
;;

(* ---------- choose a market day ---------- *)

let month_title month year =
  let name =
    match (month : Month.t) with
    | Jan -> "January"
    | Feb -> "February"
    | Mar -> "March"
    | Apr -> "April"
    | May -> "May"
    | Jun -> "June"
    | Jul -> "July"
    | Aug -> "August"
    | Sep -> "September"
    | Oct -> "October"
    | Nov -> "November"
    | Dec -> "December"
  in
  sprintf "%s %d" name year
;;

(* The weekday (Mon-Fri) cells of one month, chunked into calendar rows;
   [None] pads the first row so day-of-week columns line up. Weekends are
   dropped entirely -- there are no weekend sessions to offer. *)
let month_weeks ~year ~month =
  let cells =
    List.filter_map
      (List.range 1 (Date.days_in_month ~year ~month + 1))
      ~f:(fun d ->
        let date = Date.create_exn ~y:year ~m:month ~d in
        let col =
          Day_of_week.iso_8601_weekday_number (Date.day_of_week date) - 1
        in
        if col > 4 then None else Some (col, date))
  in
  match cells with
  | [] -> []
  | (first_col, (_ : Date.t)) :: (_ : (int * Date.t) list) ->
    List.chunks_of
      ~length:5
      (List.init first_col ~f:(fun (_ : int) -> None)
       @ List.map cells ~f:(fun ((_ : int), date) -> Some date))
;;

let choose_day_view
  ~theme
  ~is_dark
  ~selection
  ~select
  ~continue_
  ~profile
  ~on_brand
  ~toggle_theme
  ~back
  =
  (* One calendar for the whole dataset: a day is pickable if any bundled
     symbol traded it. Which symbols a run touches is the alpha's decision,
     so the picker stops asking. *)
  let sessions =
    List.concat_map Dataset.symbols ~f:(fun symbol ->
      Dataset.dates_for symbol)
    |> List.dedup_and_sort ~compare:Date.compare
  in
  let session_set = Date.Set.of_list sessions in
  let symbols_on date =
    List.filter Dataset.symbols ~f:(fun symbol ->
      List.mem (Dataset.dates_for symbol) date ~equal:Date.equal)
  in
  let hint =
    Styles.s
      ("color:" ^ theme.Styles.faint ^ ";font-size:12px;" ^ Styles.mono)
  in
  let cell_base =
    "width:56px;height:42px;display:flex;align-items:center;justify-content:center;border-radius:3px;font-size:13.5px;"
    ^ Styles.mono
  in
  let cell date_opt =
    match date_opt with
    | None -> {%html|<span></span>|}
    | Some date ->
      let day = Int.to_string (Date.day date) in
      if not (Set.mem session_set date)
      then (
        let dim =
          Styles.s (cell_base ^ "color:" ^ theme.Styles.faint ^ ";")
        in
        {%html|<span %{dim}>#{day}</span>|})
      else (
        let selected =
          match selection with Some d -> Date.equal d date | None -> false
        in
        let bg =
          if selected then theme.Styles.blue else theme.Styles.chip_bg
        in
        let color =
          if selected then theme.Styles.page_bg else theme.Styles.text
        in
        let border =
          if selected then theme.Styles.blue else theme.Styles.chip_border
        in
        let ring =
          if selected
          then "box-shadow:0 2px 8px " ^ theme.Styles.blue ^ "66;"
          else ""
        in
        let style =
          Styles.s
            (cell_base
             ^ "background:"
             ^ bg
             ^ ";color:"
             ^ color
             ^ ";border:1px solid "
             ^ border
             ^ ";cursor:pointer;font-weight:600;"
             ^ ring)
        in
        {%html|
          <button
            class="btn cell-pop"
            %{style}
            title=%{Date.to_string date}
            on_click=%{fun _ -> select date}>
            #{day}
          </button>
        |})
  in
  let weekday_header =
    let head =
      Styles.s
        (cell_base
         ^ "height:auto;color:"
         ^ theme.Styles.faint
         ^ ";font-size:11px;")
    in
    List.map [ "Mon"; "Tue"; "Wed"; "Thu"; "Fri" ] ~f:(fun name ->
      {%html|<span %{head}>#{name}</span>|})
  in
  let month_grid (year, month) =
    let title =
      Styles.s
        ("color:"
         ^ theme.Styles.text
         ^ ";font-size:13px;font-weight:700;margin:14px 0 8px;")
    in
    let grid =
      Styles.s "display:grid;grid-template-columns:repeat(5,56px);gap:4px;"
    in
    let rows = List.concat_map (month_weeks ~year ~month) ~f:Fn.id in
    {%html|
      <div>
        <div %{title}>#{month_title month year}</div>
        <div %{grid}>
          *{weekday_header}
          *{List.map rows ~f:cell}
        </div>
      </div>
    |}
  in
  let months =
    sessions
    |> List.map ~f:(fun date -> Date.year date, Date.month date)
    |> List.dedup_and_sort ~compare:[%compare: int * Month.t]
  in
  (* The whole market day at a glance: one row per symbol that traded it — a
     small sparkline and the session's vitals. *)
  let day_preview =
    let empty message =
      {%html|
        <div
          %{Styles.card
              theme
              "padding:24px;display:flex;flex-direction:column;align-items:center;justify-content:center;gap:10px;min-height:300px;"}>
          <span %{Styles.s ("color:" ^ theme.Styles.faint ^ ";")}>
            %{Icon.calendar ~size:30 ()}
          </span>
          <span
            %{Styles.s
                ("color:"
                 ^ theme.Styles.secondary
                 ^ ";font-size:14px;font-weight:600;")}>
            #{message}
          </span>
          <span
            %{Styles.s
                ("color:" ^ theme.Styles.faint ^ ";font-size:12px;")}>
            solid dates have bundled market data
          </span>
        </div>
      |}
    in
    match selection with
    | None -> empty "Pick a market day to preview it"
    | Some date ->
      let symbol_row symbol =
        match Dataset.load_cached ~symbol ~date with
        | Error (_ : Error.t) -> None
        | Ok day ->
          let bars = day.Trading_day.bars in
          let closes =
            Array.of_list_map bars ~f:(fun bar ->
              Price.to_float bar.Market_bar.close)
          in
          let n = Array.length closes in
          let open_ = Price.to_float (List.hd_exn bars).Market_bar.open_ in
          let close = closes.(n - 1) in
          let volume =
            List.sum (module Int) bars ~f:(fun bar ->
              Size.to_int bar.Market_bar.volume)
          in
          let move_bps = (close -. open_) /. open_ *. 10000. in
          let spark =
            let w = 150. in
            let h = 34. in
            let lo = Array.fold closes ~init:Float.infinity ~f:Float.min in
            let hi =
              Array.fold closes ~init:Float.neg_infinity ~f:Float.max
            in
            let span = Float.max (hi -. lo) 0.01 in
            let pts =
              List.filter_map (List.init n ~f:Fn.id) ~f:(fun i ->
                if i % 6 = 0 || i = n - 1
                then
                  Some
                    (sprintf
                       "%.1f,%.1f"
                       (Float.of_int i /. Float.of_int (n - 1) *. w)
                       (2. +. ((hi -. closes.(i)) /. span *. (h -. 4.))))
                else None)
              |> String.concat ~sep:" "
            in
            Vdom.Node.create_svg
              "svg"
              ~attrs:
                [ Vdom.Attr.create "viewBox" (sprintf "0 0 %.0f %.0f" w h)
                ; Styles.s "width:150px;height:34px;display:block;"
                ]
              [ Vdom.Node.create_svg
                  "polyline"
                  ~attrs:
                    [ Vdom.Attr.create "points" pts
                    ; Vdom.Attr.create "fill" "none"
                    ; Vdom.Attr.create "stroke" theme.Styles.blue
                    ; Vdom.Attr.create "stroke-width" "1.4"
                    ]
                  []
              ]
          in
          let row =
            Styles.s
              ("display:grid;grid-template-columns:64px 150px 1fr 1fr 1fr \
                1fr;gap:12px;align-items:center;padding:9px \
                0;border-bottom:1px solid "
               ^ theme.Styles.hairline
               ^ ";font-size:12.5px;color:"
               ^ theme.Styles.text
               ^ ";"
               ^ Styles.mono)
          in
          let num = Styles.s "text-align:right;" in
          Some
            {%html|
              <div %{row}>
                <span %{Styles.s "font-weight:700;"}>
                  #{Symbol.to_string symbol}
                </span>
                %{spark}
                <span %{num}>#{sprintf "$%.2f" open_}</span>
                <span %{num}>#{sprintf "$%.2f" close}</span>
                <span %{num}>#{sprintf "%+.0f bps" move_bps}</span>
                <span %{num}>
                  #{Float.to_string_hum ~delimiter:',' ~decimals:0
                      (Float.of_int volume)}
                </span>
              </div>
            |}
      in
      let rows = List.filter_map (symbols_on date) ~f:symbol_row in
      let title_row =
        Styles.s
          ("display:flex;justify-content:space-between;align-items:baseline;gap:14px;flex-wrap:wrap;color:"
           ^ theme.Styles.text
           ^ ";font-size:16px;font-weight:700;")
      in
      let head =
        Styles.s
          ("display:grid;grid-template-columns:64px 150px 1fr 1fr 1fr \
            1fr;gap:12px;padding:12px 0 6px;border-bottom:1px solid "
           ^ theme.Styles.hairline
           ^ ";"
           ^ Styles.table_label theme)
      in
      let num = Styles.s "text-align:right;" in
      {%html|
        <div %{Styles.card theme "padding:20px;"}>
          <div %{title_row}>
            <span %{Styles.s "white-space:nowrap;"}>
              #{sprintf "%s · %d %s trading"
                  (Date.to_string date)
                  (List.length rows)
                  (if List.length rows = 1 then "symbol" else "symbols")}
            </span>
            <span %{hint}>
              #{if List.length rows = 1
                then "your alpha trades this symbol"
                else "your alpha may name any of them — or several"}
            </span>
          </div>
          <div %{head}>
            <span>symbol</span>
            <span>session</span>
            <span %{num}>open</span>
            <span %{num}>close</span>
            <span %{num}>day move</span>
            <span %{num}>volume</span>
          </div>
          *{rows}
        </div>
      |}
  in
  {%html|
    <div class="page fade" %{Styles.s narrow_page}>
      %{wizard_header ~step:0 ?profile ~on_brand ~theme ~is_dark ~toggle_theme
          ~title:"Choose a market day"
          ~subtitle:"pick a real historical trading day"
          ~back:None ()}
      <div %{Styles.s two_col}>
        <div %{Styles.card theme "padding:20px;"}>
          <div %{Styles.s "display:flex;gap:12px;align-items:center;"}>
            <span %{hint}>
              #{sprintf "%d market days available" (List.length sessions)}
            </span>
          </div>
          *{List.map months ~f:month_grid}
        </div>
        %{day_preview}
      </div>
      %{nav_footer ~theme
          ~back:("Dashboard", back)
          ~next:("Continue", continue_, Option.is_some selection) ()}
    </div>
  |}
;;

(* ---------- alpha upload ---------- *)

(* Canned instruction sets, generated for the chosen day's symbol so a sample
   never trips the symbol-match check. Sizes stay demo-scale (see the POV
   default-rate note in bin/main.ml). *)
(* Other names with a session on this date, so a basket sample can only name
   symbols the run can actually load. *)
let others_trading ~symbol ~date =
  List.filter Dataset.symbols ~f:(fun other ->
    (not (Symbol.equal other symbol))
    && List.mem (Dataset.dates_for other) date ~equal:Date.equal)
;;

(* Gross alpha for one leg is exactly
   [side * quantity * (close - arrival price)], so the sign of a name's move
   from the order's arrival to the close is what decides whether a sample's
   prediction was right. Reading it off the real session lets a sample stand
   in for a model that called that day correctly (or, deliberately, one that
   did not) — which is the point: the platform grades execution, and a sample
   whose paper alpha is roughly zero grades it against nothing. *)
let move_to_close ~symbol ~date ~at =
  match Dataset.load_cached ~symbol ~date with
  | Error (_ : Error.t) -> 0.
  | Ok day ->
    (match
       List.find day.Trading_day.bars ~f:(fun bar ->
         Time_ns.Ofday.( >= ) bar.Market_bar.time at)
     with
     | None -> 0.
     | Some bar ->
       Price.to_float (List.last_exn day.Trading_day.bars).Market_bar.close
       -. Price.to_float bar.Market_bar.open_)
;;

let price_at ~symbol ~date ~at =
  match Dataset.load_cached ~symbol ~date with
  | Error (_ : Error.t) -> 0.
  | Ok day ->
    (match
       List.find day.Trading_day.bars ~f:(fun bar ->
         Time_ns.Ofday.( >= ) bar.Market_bar.time at)
     with
     | None -> 0.
     | Some bar -> Price.to_float bar.Market_bar.open_)
;;

let ofday = Time_ns.Ofday.of_string
let buy_if positive = if positive then "BUY" else "SELL"

(* A sample is either a model that read the day right or one that read it
   backwards. Every leg of a sample takes the same view, so its legs never
   cancel: gross alpha is the sum of [quantity * |move|] with one sign, which
   keeps the paper alpha decisively large and the capture ratio meaningful.
   Mixing the two within a sample is what produces the near-zero denominators
   that make capture read in the thousands of percent. *)
type view =
  | Right
  | Wrong

let side_for view ~symbol ~date ~at =
  let move = move_to_close ~symbol ~date ~at in
  match view with
  | Right -> buy_if (Float.( >= ) move 0.)
  | Wrong -> buy_if (Float.( < ) move 0.)
;;

let sample_alphas ~symbol ~date =
  let s = Symbol.to_string symbol in
  (* [view] fixes whether this sample's model was right; each leg's side then
     follows that name's own move to the close. *)
  let csv view rows =
    String.concat
      ~sep:"\n"
      (List.map rows ~f:(fun (arrival, quantity, deadline) ->
         sprintf
           "%s,%s,%s,%d,%s"
           arrival
           s
           (side_for view ~symbol ~date ~at:(ofday arrival))
           quantity
           deadline))
    ^ "\n"
  in
  (* A round trip is the one shape whose second leg must oppose the first —
     it is closing the position, not taking another view. Its gross alpha is
     [quantity * (price out - price in)], so the direction that makes the
     trip profitable is the direction the name actually moved between the two
     times. *)
  let round_trip ~quantity ~in_at ~in_by ~out_at ~out_by =
    let rose =
      Float.( >= )
        (price_at ~symbol ~date ~at:(ofday out_at))
        (price_at ~symbol ~date ~at:(ofday in_at))
    in
    let first = buy_if rose in
    let second = buy_if (not rose) in
    sprintf
      "%s,%s,%s,%d,%s\n%s,%s,%s,%d,%s\n"
      in_at
      s
      first
      quantity
      in_by
      out_at
      s
      second
      quantity
      out_by
  in
  [ ( "A typical day"
    , "Three orders through the session — one view, read off what the name \
       actually did. Start here."
    , csv
        Right
        [ "10:00:00", 5000, "11:00:00"
        ; "11:30:00", 3000, "13:00:00"
        ; "14:00:00", 2000, "14:30:00"
        ] )
  ; ( "A working morning"
    , "8,000 shares as three overlapping orders, all finished by 12:30."
    , csv
        Right
        [ "09:45:00", 3000, "11:00:00"
        ; "10:15:00", 3000, "12:00:00"
        ; "11:00:00", 2000, "12:30:00"
        ] )
  ; ( "Round trip"
    , "In at 10:00, back out at 13:00 — the same 6,000 shares, so the paper \
       profit is just the move between the two."
    , round_trip
        ~quantity:6000
        ~in_at:"10:00:00"
        ~in_by:"11:30:00"
        ~out_at:"13:00:00"
        ~out_by:"15:30:00" )
  ; ( "Into the close"
    , "8,000 shares finishing at 15:55 — and a model that read the day \
       backwards, so you can see what negative alpha costs."
    , csv
        Wrong
        [ "13:00:00", 2500, "14:30:00"
        ; "13:45:00", 2500, "15:00:00"
        ; "14:30:00", 3000, "15:55:00"
        ] )
  ; ( "Busy day, six orders"
    , "Six orders from 09:40 to 15:45 — the crowded end of what one name \
       can carry."
    , csv
        Right
        [ "09:40:00", 1500, "10:30:00"
        ; "10:20:00", 1000, "11:15:00"
        ; "11:00:00", 2000, "12:30:00"
        ; "12:15:00", 1500, "13:45:00"
        ; "13:30:00", 1000, "14:30:00"
        ; "14:45:00", 2000, "15:45:00"
        ] )
  ; ( "Three at once"
    , "Three orders live together from 11:15 to 12:00 — overlapping windows \
       in one book."
    , csv
        Right
        [ "10:30:00", 3000, "12:30:00"
        ; "11:00:00", 2000, "12:00:00"
        ; "11:15:00", 2500, "13:00:00"
        ] )
  ]
  @
  (* Only offered when the day actually has other names to trade: a sample
     that named a symbol with no session for this date would fail on the
     first run, which is a poor introduction. *)
  match others_trading ~symbol ~date with
  | [] -> []
  | others ->
    let names = symbol :: List.take others 2 in
    let rows =
      List.mapi names ~f:(fun index name ->
        let arrival, quantity, deadline =
          match index with
          | 0 -> "10:00:00", 5000, "11:30:00"
          | 1 -> "10:15:00", 4000, "12:00:00"
          | (_ : int) -> "10:45:00", 3000, "12:30:00"
        in
        sprintf
          "%s,%s,%s,%d,%s"
          arrival
          (Symbol.to_string name)
          (side_for Right ~symbol:name ~date ~at:(ofday arrival))
          quantity
          deadline)
    in
    [ ( sprintf "%d names at once" (List.length names)
      , sprintf
          "One basket across %s, each name called the way it actually went. \
           Every name gets its own book and its own benchmarks; the chart \
           has a tab per symbol."
          (String.concat ~sep:", " (List.map names ~f:Symbol.to_string))
      , String.concat ~sep:"\n" rows ^ "\n" )
    ]
;;

(* Reads the first selected file as text and pours it into the alpha
   textarea. The FileReader callback fires outside the DOM event, so the
   state update is scheduled via [handle_non_dom_event_exn]. *)
let read_alpha_file ~set_alpha_text files =
  let open Js_of_ocaml in
  Effect.of_sync_fun
    (fun () ->
      Js.Opt.iter
        (files##item 0)
        (fun file ->
          let reader = new%js File.fileReader in
          reader##.onload
          := Dom.handler
               (fun (_ : File.fileReader File.progressEvent Js.t) ->
                  (match
                     Js.Opt.to_option (File.CoerceTo.string reader##.result)
                   with
                   | None -> ()
                   | Some text ->
                     Vdom.Effect.Expert.handle_non_dom_event_exn
                       (set_alpha_text (Js.to_string text)));
                  Js._true);
          reader##readAsText file))
    ()
;;

let alpha_view
  ~theme
  ~is_dark
  ~date
  ~alpha_text
  ~set_alpha_text
  ~continue_
  ~profile
  ~on_brand
  ~toggle_theme
  ~back
  =
  (* Samples are written against a lead symbol; the first name trading this
     date serves. Multi-symbol samples pull in the others themselves. *)
  let symbol =
    List.find Dataset.symbols ~f:(fun candidate ->
      List.mem (Dataset.dates_for candidate) date ~equal:Date.equal)
    |> Option.value ~default:(Symbol.of_string "TSLA")
  in
  let error_style =
    Styles.s
      ("color:" ^ theme.Styles.red ^ ";font-size:12.5px;" ^ Styles.mono)
  in
  let textarea_style =
    Styles.s
      ("width:100%;box-sizing:border-box;background:"
       ^ theme.Styles.page_bg
       ^ ";color:"
       ^ theme.Styles.text
       ^ ";border:1px solid "
       ^ theme.Styles.chip_border
       ^ ";border-radius:3px;padding:10px;font-size:13px;resize:vertical;line-height:1.6;"
       ^ Styles.mono)
  in
  let parsed = Replay.parse_alpha alpha_text in
  (* "3 rows parsed · 0 errors", green when clean, red when not. *)
  let parse_status =
    match parsed with
    | Ok instructions ->
      let style =
        Styles.s
          ("color:"
           ^ theme.Styles.green
           ^ ";font-size:11.5px;font-weight:600;"
           ^ Styles.mono)
      in
      {%html|
        <span %{style}>
          #{sprintf "%d rows parsed · 0 errors" (List.length instructions)}
        </span>
      |}
    | Error (_ : Error.t) ->
      let style =
        Styles.s
          ("color:"
           ^ theme.Styles.red
           ^ ";font-size:11.5px;font-weight:600;"
           ^ Styles.mono)
      in
      {%html|<span %{style}>parse errors — details on the right</span>|}
  in
  (* The right-hand panel: the parsed orders as a table, or the errors. *)
  let preview =
    let validity =
      match parsed with
      | Ok (_ : Alpha_instruction.t list) ->
        let style =
          Styles.s
            ("color:"
             ^ theme.Styles.green
             ^ ";font-size:11px;font-weight:700;"
             ^ Styles.mono)
        in
        {%html|<span %{style}>✓ valid</span>|}
      | Error (_ : Error.t) ->
        let style =
          Styles.s
            ("color:"
             ^ theme.Styles.red
             ^ ";font-size:11px;font-weight:700;"
             ^ Styles.mono)
        in
        {%html|<span %{style}>✗ invalid</span>|}
    in
    let head =
      {%html|
        <div
          %{Styles.s
              "display:flex;justify-content:space-between;align-items:baseline;gap:12px;margin-bottom:6px;"}>
          <span %{Styles.s (Styles.label theme)}>Parsed instructions</span>
          %{validity}
        </div>
      |}
    in
    let body =
      match parsed with
      | Ok instructions ->
        instructions_header ~theme
        :: List.map instructions ~f:(instruction_row ~theme)
      | Error error ->
        [ {%html|<div %{error_style}>#{Error.to_string_hum error}</div>|} ]
    in
    let explain =
      Styles.s
        ("color:"
         ^ theme.Styles.faint
         ^ ";font-size:11.5px;line-height:1.6;margin-top:10px;")
    in
    {%html|
      <div %{Styles.card theme "padding:20px;"}>
        %{head}
        *{body}
        <div %{explain}>
          Each instruction is a parent order: arrive at a time, finish by a
          deadline. Malformed rows are flagged with their line number.
        </div>
      </div>
    |}
  in
  (* Samples as the mockup's list: name and description on the left, the
     order count on the right, the selected one inked in blue. *)
  let sample_row (name, description, csv) =
    let selected = String.equal alpha_text csv in
    let orders =
      List.count (String.split_lines csv) ~f:(fun line ->
        not (String.is_empty (String.strip line)))
    in
    let style =
      Styles.s
        ("display:flex;justify-content:space-between;align-items:flex-start;gap:12px;text-align:left;width:100%;background:"
         ^ (if selected then theme.Styles.blue_soft else "transparent")
         ^ ";border:1px solid "
         ^ (if selected then theme.Styles.blue else theme.Styles.chip_border)
         ^ ";border-left:3px solid "
         ^ (if selected then theme.Styles.blue else theme.Styles.chip_border)
         ^ ";border-radius:3px;padding:9px 12px;cursor:pointer;")
    in
    let name_style =
      Styles.s
        ("font-size:12.5px;font-weight:700;color:"
         ^ (if selected then theme.Styles.blue else theme.Styles.text)
         ^ ";")
    in
    let desc_style =
      Styles.s
        ("font-size:11.5px;line-height:1.5;color:"
         ^ theme.Styles.secondary
         ^ ";")
    in
    let count_style =
      Styles.s
        ("color:"
         ^ theme.Styles.faint
         ^ ";font-size:11px;white-space:nowrap;"
         ^ Styles.mono)
    in
    {%html|
      <button class="btn" %{style} on_click=%{fun _ -> set_alpha_text csv}>
        <span %{Styles.s "display:flex;flex-direction:column;gap:2px;"}>
          <span %{name_style}>#{name}</span>
          <span %{desc_style}>#{description}</span>
        </span>
        <span %{count_style}>#{sprintf "%d orders" orders}</span>
      </button>
    |}
  in
  let upload_label =
    Styles.s
      ("display:inline-flex;align-items:center;gap:8px;background:"
       ^ theme.Styles.card_bg
       ^ ";color:"
       ^ theme.Styles.text
       ^ ";border:1px solid "
       ^ theme.Styles.chip_border
       ^ ";border-radius:3px;padding:6px \
          12px;cursor:pointer;font-size:12.5px;font-weight:600;")
  in
  let editor_head =
    let hint =
      Styles.s
        ("color:" ^ theme.Styles.brown ^ ";font-size:11px;" ^ Styles.mono)
    in
    {%html|
      <div
        %{Styles.s
            "display:flex;justify-content:space-between;align-items:baseline;gap:12px;margin:16px 0 6px;"}>
        <span %{Styles.s (Styles.label theme)}>Alpha CSV</span>
        <span %{hint}>arrival_time,symbol,side,quantity,deadline</span>
      </div>
    |}
  in
  let subtitle =
    sprintf
      "paste, upload, or start from a sample — timestamped parent orders \
       for %s, in as many names as the day has sessions for"
      (Date.to_string date)
  in
  let samples_note =
    Styles.s
      ("color:"
       ^ theme.Styles.faint
       ^ ";font-size:11px;margin-bottom:8px;"
       ^ Styles.mono)
  in
  {%html|
    <div class="page fade" %{Styles.s narrow_page}>
      %{wizard_header ~step:1 ?profile ~on_brand ~theme ~is_dark ~toggle_theme
          ~title:"Alpha instructions" ~subtitle
          ~back:None ()}
      <div %{Styles.s two_col}>
        <div %{Styles.card theme "padding:20px;"}>
          <div
            %{Styles.s
                "display:flex;justify-content:space-between;align-items:baseline;gap:12px;margin-bottom:8px;"}>
            <span %{Styles.s (Styles.label theme)}>
              Samples — written for #{Symbol.to_string symbol}
            </span>
            <span %{samples_note}>sides read off this session — a stand-in
              for a model that called it</span>
          </div>
          <div %{Styles.s "display:flex;flex-direction:column;gap:6px;"}>
            *{List.map (sample_alphas ~symbol ~date) ~f:sample_row}
          </div>
          %{editor_head}
          <textarea
            rows=%{14}
            %{Vdom.Attr.create "spellcheck" "false"}
            %{Vdom.Attr.string_property "value" alpha_text}
            %{textarea_style}
            on_input=%{fun (_ : _) text -> set_alpha_text text}></textarea>
          <div
            %{Styles.s
                "display:flex;justify-content:space-between;align-items:center;gap:12px;margin-top:10px;flex-wrap:wrap;"}>
            <label class="btn" %{upload_label}>
              %{Icon.upload ~size:14 ()}
              Upload CSV file…
              <input
                type="file"
                %{Vdom.Attr.create "accept" ".csv,text/csv"}
                %{Styles.s "display:none;"}
                %{Vdom.Attr.on_file_input (fun (_ : _) files ->
                    read_alpha_file ~set_alpha_text files)} />
            </label>
            %{parse_status}
          </div>
        </div>
        <div %{Styles.s "display:flex;flex-direction:column;gap:16px;"}>
          %{preview}
        </div>
      </div>
      %{nav_footer ~theme
          ~back:("Choose day", back)
          ~next:("Continue", continue_, Or_error.is_ok parsed) ()}
    </div>
  |}
;;

(* ---------- algorithm + confirm ---------- *)

(* ---------- algorithm reference cards ---------- *)

(* Each algorithm as a scannable card instead of a paragraph: the shape of
   the bet, where it wins, where it breaks, and who actually runs it. The
   accent color is the card's identity across the setup screen. *)
let algorithm_cards =
  [ ( "twap"
    , "TWAP"
    , "Equal slices on the clock"
    , [ "Calm, steady market"
      ; "Large order, plenty of time"
      ; "You want a predictable finish"
      ]
    , [ "Volume swings hard"; "Price trends against you all day" ]
    , "The default institutional slicer — boring on purpose." )
  ; ( "vwap"
    , "VWAP"
    , "Follows the forecast volume curve"
    , [ "The day looks like a normal day"
      ; "You are judged against the day's average price"
      ]
    , [ "News breaks and volume stops matching history" ]
    , "Desks filling a benchmark order they will be graded on." )
  ; ( "pov"
    , "POV"
    , "A fixed share of whatever actually trades"
    , [ "You care about hiding in real volume"
      ; "Liquidity is unpredictable"
      ]
    , [ "The tape dries up and you never finish"
      ; "You need a guaranteed completion time"
      ]
    , "Traders who would rather be late than obvious." )
  ; ( "is"
    , "IS"
    , "Balances impact now against drift later"
    , [ "The price is moving and waiting is risky"
      ; "You want front-loading without dumping"
      ]
    , [ "Very quiet markets, where the urgency premium is wasted" ]
    , "Anyone with a short-lived signal to capture." )
  ; ( "adaptive"
    , "Adaptive"
    , "Rests when it can, crosses when it must"
    , [ "The spread is worth more than the wait"
      ; "You have time and want to be paid for it"
      ; "Liquidity comes to you if you let it"
      ]
    , [ "A market running away from you — patience gets repriced into \
         crossing anyway"
      ; "Very short windows, where there is no time to be patient"
      ]
    , "The only one here that posts rather than pays; the rest are pure \
       takers." )
  ; ( "immediate"
    , "Immediate"
    , "Everything at once, right now"
    , [ "The signal decays in minutes"
      ; "The order is small next to the volume"
      ]
    , [ "Large orders — you pay the whole impact yourself" ]
    , "The baseline every other run is scored against." )
  ]
;;

(* Spelled out for a display heading; the numeral is the fallback rather than
   a table of every English number, since this counts a list that grows one
   entry at a time. *)
let in_words = function
  | 4 -> "Four"
  | 5 -> "Five"
  | 6 -> "Six"
  | 7 -> "Seven"
  | 8 -> "Eight"
  | (n : int) -> Int.to_string n
;;

let algorithm_detail ~theme ~algo =
  match
    List.find algorithm_cards ~f:(fun (key, _, _, _, _, _) ->
      String.equal key algo)
  with
  | None -> []
  | Some ((_ : string), name, tagline, best, weak, typical) ->
    let bullet ~color ~mark text =
      let row =
        Styles.s
          "display:flex;gap:7px;align-items:flex-start;font-size:12px;line-height:1.55;"
      in
      let mark_style =
        Styles.s ("color:" ^ color ^ ";font-weight:800;flex-shrink:0;")
      in
      let text_style = Styles.s ("color:" ^ theme.Styles.secondary ^ ";") in
      {%html|
        <div %{row}>
          <span %{mark_style}>#{mark}</span>
          <span %{text_style}>#{text}</span>
        </div>
      |}
    in
    let group ~heading ~head_color ~color ~mark items =
      let head =
        Styles.s (Styles.label theme ^ "color:" ^ head_color ^ ";")
      in
      {%html|
        <div %{Styles.s "display:flex;flex-direction:column;gap:6px;"}>
          <span %{head}>#{heading}</span>
          *{List.map items ~f:(bullet ~color ~mark)}
        </div>
      |}
    in
    let name_style =
      Styles.s
        ("color:"
         ^ theme.Styles.blue
         ^ ";font-size:15px;font-weight:800;"
         ^ Styles.mono)
    in
    let tagline_style =
      Styles.s
        ("color:" ^ theme.Styles.text ^ ";font-size:13px;font-weight:600;")
    in
    let typical_style =
      Styles.s
        ("color:"
         ^ theme.Styles.faint
         ^ ";font-size:12px;line-height:1.55;border-top:1px solid "
         ^ theme.Styles.hairline
         ^ ";padding-top:10px;")
    in
    [ {%html|
        <div
          class="fade"
          %{Styles.s
              ("display:flex;flex-direction:column;gap:12px;border-top:1px \
                solid "
               ^ theme.Styles.hairline
               ^ ";padding-top:14px;margin-top:14px;")}>
          <div %{Styles.s "display:flex;gap:10px;align-items:baseline;"}>
            <span %{name_style}>#{name}</span>
            <span %{tagline_style}>#{tagline}</span>
          </div>
          <div
            %{Styles.s
                "display:grid;grid-template-columns:repeat(auto-fit,minmax(180px,1fr));gap:14px;"}>
            %{group ~heading:"Best when" ~head_color:theme.Styles.green
                ~color:theme.Styles.green ~mark:"\u{2713}" best}
            %{group ~heading:"Weak when" ~head_color:theme.Styles.red
                ~color:theme.Styles.red ~mark:"\u{2717}" weak}
          </div>
          <div %{typical_style}>Typically used by: #{typical}</div>
        </div>
      |}
    ]
;;

(* ---------- parameter presets ---------- *)

(* Plain-language stances that fill in the algorithm knobs. They move the
   strategy's own dials only — the fill-model numbers are the market, not the
   trader, so a preset never touches those. *)
let presets =
  [ "Passive", "trade slowly, hide in the volume", 0.0008, 0.0
  ; "Balanced", "a middle-of-the-road pace", 0.0015, 1.0
  ; "Aggressive", "front-load and finish early", 0.0040, 5.0
  ]
;;

(* The posture column: a vertical list in the mockup's voice, the selected
   stance inked with a blue rail. *)
let preset_rows ~theme ~param_text ~set_param_text =
  let current_pov = param_text.Replay.Param_text.pov_rate in
  let current_urgency = param_text.Replay.Param_text.urgency in
  let matches (_, _, pov, urgency) =
    String.equal current_pov (sprintf "%.4f" pov)
    && String.equal current_urgency (sprintf "%.1f" urgency)
  in
  let is_custom = not (List.exists presets ~f:matches) in
  let item ~selected ~title ~subtitle ~on_click =
    let style =
      Styles.s
        ("display:flex;flex-direction:column;gap:1px;text-align:left;background:"
         ^ (if selected then theme.Styles.blue_soft else "transparent")
         ^ ";border:none;border-left:3px solid "
         ^ (if selected then theme.Styles.blue else theme.Styles.hairline)
         ^ ";padding:7px 10px;cursor:pointer;width:100%;")
    in
    let title_style =
      Styles.s
        ("font-size:12.5px;font-weight:700;color:"
         ^ (if selected then theme.Styles.blue else theme.Styles.text)
         ^ ";")
    in
    let sub_style =
      Styles.s ("font-size:11px;color:" ^ theme.Styles.secondary ^ ";")
    in
    {%html|
      <button class="btn" %{style} on_click=%{on_click}>
        <span %{title_style}>#{title}</span>
        <span %{sub_style}>#{subtitle}</span>
      </button>
    |}
  in
  let preset_item (name, blurb, pov, urgency) =
    item
      ~selected:(matches (name, blurb, pov, urgency))
      ~title:name
      ~subtitle:blurb
      ~on_click:(fun _ ->
        set_param_text
          { param_text with
            Replay.Param_text.pov_rate = sprintf "%.4f" pov
          ; urgency = sprintf "%.1f" urgency
          })
  in
  let note =
    Styles.s
      ("color:"
       ^ theme.Styles.faint
       ^ ";font-size:10.5px;line-height:1.5;margin-top:8px;")
  in
  {%html|
    <div>
      <div %{Styles.s (Styles.label theme ^ "margin-bottom:8px;")}>
        Strategy posture · yours
      </div>
      <div %{Styles.s "display:flex;flex-direction:column;gap:4px;"}>
        *{List.map presets ~f:preset_item}
        %{item ~selected:is_custom ~title:"Custom"
            ~subtitle:"your own numbers below"
            ~on_click:(fun _ -> Effect.Ignore)}
      </div>
      <div %{note}>
        Presets set strategy dials only — they never alter market physics.
      </div>
    </div>
  |}
;;

let setup_view
  ~theme
  ~is_dark
  ~open_friction_help
  ~date
  ~alpha_text
  ~algo
  ~set_algo
  ~param_text
  ~set_param_text
  ~start
  ~run_error
  ~profile
  ~on_brand
  ~toggle_theme
  ~back
  =
  let section_label = Styles.s (Styles.label theme ^ "margin-bottom:8px;") in
  let error_style =
    Styles.s
      ("color:" ^ theme.Styles.red ^ ";font-size:12.5px;" ^ Styles.mono)
  in
  let error_card =
    match run_error with
    | None -> []
    | Some error ->
      [ {%html|
          <div %{Styles.card theme "padding:16px;"}>
            <div %{section_label}>Run failed</div>
            <div %{error_style}>#{Error.to_string_hum error}</div>
          </div>
        |}
      ]
  in
  let parsed = Replay.parse_alpha alpha_text in
  (* The mockup's algorithm chooser: a vertical list, name plus tagline, with
     the selected algorithm's reference card unfolding beneath. *)
  let algo_item
    (key, name, tagline, (_ : string list), (_ : string list), (_ : string))
    =
    let selected = String.equal algo key in
    let style =
      Styles.s
        ("display:flex;flex-direction:column;gap:1px;text-align:left;width:100%;background:"
         ^ (if selected then theme.Styles.blue_soft else "transparent")
         ^ ";border:1px solid "
         ^ (if selected then theme.Styles.blue else theme.Styles.chip_border)
         ^ ";border-radius:3px;padding:8px 12px;cursor:pointer;")
    in
    let name_style =
      Styles.s
        ("font-size:12.5px;font-weight:800;color:"
         ^ (if selected then theme.Styles.blue else theme.Styles.text)
         ^ ";"
         ^ Styles.mono)
    in
    let tagline_style =
      Styles.s ("font-size:11.5px;color:" ^ theme.Styles.secondary ^ ";")
    in
    {%html|
      <button class="btn" %{style} on_click=%{fun _ -> set_algo key}>
        <span %{name_style}>#{name}</span>
        <span %{tagline_style}>#{tagline}</span>
      </button>
    |}
  in
  (* The right panel: what this run will actually trade, validated. *)
  let instructions_panel =
    let body =
      match parsed with
      | Ok instructions ->
        instructions_header ~theme
        :: List.map instructions ~f:(instruction_row ~theme)
      | Error error ->
        [ {%html|<div %{error_style}>#{Error.to_string_hum error}</div>|} ]
    in
    let summary =
      match parsed with
      | Error (_ : Error.t) -> []
      | Ok instructions ->
        let names =
          List.map instructions ~f:(fun instruction ->
            instruction.Alpha_instruction.symbol)
          |> List.dedup_and_sort ~compare:Symbol.compare
          |> List.map ~f:Symbol.to_string
          |> String.concat ~sep:" "
        in
        let shares =
          List.sum (module Int) instructions ~f:(fun instruction ->
            Size.to_int instruction.Alpha_instruction.quantity)
        in
        let line =
          Styles.s
            ("color:"
             ^ theme.Styles.faint
             ^ ";font-size:11px;margin-top:10px;"
             ^ Styles.mono)
        in
        let ready =
          Styles.s
            ("border:1px solid "
             ^ theme.Styles.green
             ^ ";border-radius:3px;padding:8px 12px;margin-top:10px;color:"
             ^ theme.Styles.green
             ^ ";font-size:11.5px;font-weight:600;"
             ^ Styles.mono)
        in
        (* Parsing is not enough: every symbol the alpha names must have a
           bundled session on this date, or the run will fail at start. *)
        let missing =
          List.map instructions ~f:(fun instruction ->
            instruction.Alpha_instruction.symbol)
          |> List.dedup_and_sort ~compare:Symbol.compare
          |> List.filter ~f:(fun symbol ->
            not (List.mem (Dataset.dates_for symbol) date ~equal:Date.equal))
        in
        let not_ready =
          Styles.s
            ("border:1px solid "
             ^ theme.Styles.red
             ^ ";border-radius:3px;padding:8px 12px;margin-top:10px;color:"
             ^ theme.Styles.red
             ^ ";font-size:11.5px;font-weight:600;"
             ^ Styles.mono)
        in
        [ {%html|
            <div %{line}>
              #{sprintf "%s · %d instructions · %s shares" names
                  (List.length instructions)
                  (Int.to_string_hum ~delimiter:','  shares)}
            </div>
          |}
        ; (match missing with
           | [] ->
             {%html|
               <div %{ready}>
                 ✓ ready — validated against session #{Date.to_string date}
               </div>
             |}
           | missing ->
             {%html|
               <div %{not_ready}>
                 #{sprintf "✗ no bundled data on %s for: %s"
                     (Date.to_string date)
                     (String.concat ~sep:" "
                        (List.map missing ~f:Symbol.to_string))}
               </div>
             |})
        ]
    in
    {%html|
      <div %{Styles.card theme "padding:20px;"}>
        <div %{section_label}>Alpha instructions</div>
        *{body}
        *{summary}
      </div>
    |}
  in
  (* A labelled field, with the mockup's "· POV only" annotations; inactive
     dials stay visible but fade. *)
  let param_field ?(active = true) ?note ~label:text ~value ~set () =
    let input_style =
      Styles.s
        ("width:110px;background:"
         ^ theme.Styles.page_bg
         ^ ";color:"
         ^ theme.Styles.text
         ^ ";border:1px solid "
         ^ theme.Styles.chip_border
         ^ ";border-radius:3px;padding:6px 9px;font-size:12.5px;"
         ^ Styles.mono)
    in
    let wrap =
      Styles.s
        ("display:flex;flex-direction:column;gap:4px;"
         ^ if active then "" else "opacity:0.45;")
    in
    let note_node =
      match note with
      | None -> []
      | Some note_text ->
        let style =
          Styles.s
            ("color:"
             ^ theme.Styles.faint
             ^ ";font-size:9.5px;letter-spacing:0.04em;"
             ^ Styles.mono)
        in
        [ {%html|<span %{style}>· #{note_text}</span>|} ]
    in
    {%html|
      <label %{wrap}>
        <span %{Styles.s "display:flex;gap:5px;align-items:baseline;"}>
          <span %{Styles.s (Styles.label theme)}>#{text}</span>
          *{note_node}
        </span>
        <input
          type="text"
          %{Vdom.Attr.string_property "value" value}
          %{input_style}
          on_input=%{fun (_ : _) v -> set v} />
      </label>
    |}
  in
  let update f value = set_param_text (f param_text value) in
  let is_synthetic =
    String.equal param_text.Replay.Param_text.engine "synthetic"
  in
  (* The fill engine as the mockup's radio list. *)
  let engine_choice =
    let option value ~name ~blurb =
      let selected =
        String.equal param_text.Replay.Param_text.engine value
      in
      let style =
        Styles.s
          ("display:flex;gap:8px;align-items:flex-start;text-align:left;width:100%;background:"
           ^ (if selected then theme.Styles.blue_soft else "transparent")
           ^ ";border:1px solid "
           ^ (if selected
              then theme.Styles.blue
              else theme.Styles.chip_border)
           ^ ";border-radius:3px;padding:8px 10px;cursor:pointer;")
      in
      let dot =
        Styles.s
          ("color:"
           ^ (if selected then theme.Styles.blue else theme.Styles.faint)
           ^ ";font-size:11px;flex-shrink:0;margin-top:1px;")
      in
      let name_style =
        Styles.s
          ("font-size:12px;font-weight:700;color:"
           ^ (if selected then theme.Styles.blue else theme.Styles.text)
           ^ ";")
      in
      let blurb_style =
        Styles.s
          ("font-size:10.5px;line-height:1.5;color:"
           ^ theme.Styles.secondary
           ^ ";")
      in
      {%html|
        <button
          class="btn"
          %{style}
          on_click=%{fun _ ->
            update (fun p v -> { p with Replay.Param_text.engine = v })
              value}>
          <span %{dot}>#{if selected then "●" else "○"}</span>
          <span %{Styles.s "display:flex;flex-direction:column;gap:1px;"}>
            <span %{name_style}>#{name}</span>
            <span %{blurb_style}>#{blurb}</span>
          </span>
        </button>
      |}
    in
    {%html|
      <div>
        <div %{Styles.s (Styles.label theme ^ "margin-bottom:8px;")}>
          Fill engine
        </div>
        <div %{Styles.s "display:flex;flex-direction:column;gap:6px;"}>
          %{option "bar" ~name:"Bar model"
              ~blurb:"deterministic fills on 1-minute bars"}
          %{option "synthetic" ~name:"Synthetic exchange"
              ~blurb:"seeded book · queue + partial fills"}
          %{param_field ~active:is_synthetic ~note:"synthetic only"
              ~label:"seed" ~value:param_text.Replay.Param_text.seed
              ~set:(update (fun p v -> { p with Replay.Param_text.seed = v }))
              ()}
        </div>
      </div>
    |}
  in
  let algo_params =
    let note =
      Styles.s
        ("color:"
         ^ theme.Styles.faint
         ^ ";font-size:10.5px;line-height:1.5;margin-top:8px;")
    in
    {%html|
      <div>
        <div %{Styles.s (Styles.label theme ^ "margin-bottom:8px;")}>
          Algorithm parameters
        </div>
        <div %{Styles.s "display:flex;flex-direction:column;gap:10px;"}>
          %{param_field ~active:(String.equal algo "pov") ~note:"POV only"
              ~label:"pov rate"
              ~value:param_text.Replay.Param_text.pov_rate
              ~set:(update (fun p v ->
                { p with Replay.Param_text.pov_rate = v })) ()}
          %{param_field ~active:(String.equal algo "is") ~note:"IS only"
              ~label:"is urgency"
              ~value:param_text.Replay.Param_text.urgency
              ~set:(update (fun p v ->
                { p with Replay.Param_text.urgency = v })) ()}
          %{param_field ~active:(String.equal algo "adaptive")
              ~note:"Adaptive only"
              ~label:"patience"
              ~value:param_text.Replay.Param_text.patience
              ~set:(update (fun p v ->
                { p with Replay.Param_text.patience = v })) ()}
        </div>
        <div %{note}>
          TWAP and VWAP follow their own schedules and ignore every dial
          here. Patience runs 0 to 1: at 0 Adaptive never rests, which is
          TWAP exactly.
        </div>
      </div>
    |}
  in
  let friction_help_button =
    let style =
      Styles.s
        ("display:inline-flex;align-items:center;justify-content:center;width:17px;height:17px;border-radius:999px;background:"
         ^ theme.Styles.chip_bg
         ^ ";color:"
         ^ theme.Styles.blue
         ^ ";border:1px solid "
         ^ theme.Styles.chip_border
         ^ ";cursor:pointer;font-size:11px;font-weight:800;flex-shrink:0;")
    in
    {%html|
      <button
        class="btn"
        %{style}
        title="What do these settings mean?"
        on_click=%{fun _ -> open_friction_help}>
        ?
      </button>
    |}
  in
  let friction_params =
    {%html|
      <div>
        <div %{Styles.s (Styles.label theme ^ "margin-bottom:8px;")}>
          Market friction
        </div>
        <div %{Styles.s "display:flex;flex-direction:column;gap:10px;"}>
          %{param_field ~label:"half spread $"
              ~value:param_text.Replay.Param_text.half_spread
              ~set:(update (fun p v ->
                { p with Replay.Param_text.half_spread = v })) ()}
          %{param_field ~label:"participation cap"
              ~value:param_text.Replay.Param_text.participation
              ~set:(update (fun p v ->
                { p with Replay.Param_text.participation = v })) ()}
          %{param_field ~label:"impact coeff $"
              ~value:param_text.Replay.Param_text.impact
              ~set:(update (fun p v ->
                { p with Replay.Param_text.impact = v })) ()}
        </div>
      </div>
    |}
  in
  let params_card =
    {%html|
      <div %{Styles.card theme "padding:20px;position:relative;"}>
        <div %{Styles.s "position:absolute;top:14px;right:14px;"}>
          %{friction_help_button}
        </div>
        <div
          %{Styles.s
              "display:grid;grid-template-columns:repeat(auto-fit,minmax(210px,1fr));gap:24px;align-items:start;"}>
          %{preset_rows ~theme ~param_text ~set_param_text}
          %{algo_params}
          %{friction_params}
          %{engine_choice}
        </div>
      </div>
    |}
  in
  {%html|
    <div class="page fade" %{Styles.s narrow_page}>
      %{wizard_header ~step:2 ?profile ~on_brand ~theme ~is_dark ~toggle_theme
          ~title:"New simulation"
          ~subtitle:(sprintf
                       "%s · %s — three separate things: the algorithm, \
                        your strategy dials, and the market's physics"
                       (alpha_symbols_line alpha_text)
                       (Date.to_string date))
          ~back:None ()}
      <div %{Styles.s two_col}>
        <div %{Styles.card theme "padding:20px;"}>
          <div %{section_label}>Execution algorithm</div>
          <div %{Styles.s "display:flex;flex-direction:column;gap:6px;"}>
            *{List.map algorithm_cards ~f:algo_item}
          </div>
          *{algorithm_detail ~theme ~algo}
        </div>
        %{instructions_panel}
      </div>
      %{params_card}
      *{error_card}
      %{nav_footer ~theme
          ~back:("Alpha", back)
          ~next:("Run simulation", start, true) ()}
    </div>
  |}
;;

(* ---------- results: the metric tree ---------- *)

let results_view
  (replay : Replay.t)
  ~theme
  ~is_dark
  ~open_help
  ~previous_run
  ~profile
  ~on_brand
  ~to_sim
  ~new_sim
  ~to_my_runs
  ~retest
  ~to_dashboard
  ~toggle_theme
  =
  let rows = replay.results.rows in
  let sum f = List.sum (module Int) rows ~f in
  let total_net =
    sum (fun row -> row.Replay.grading.Transaction_cost.net_pnl_cents)
  in
  let total_gross =
    sum (fun row -> row.Replay.grading.gross_theoretical_pnl_cents)
  in
  let capture_ratio =
    if total_gross > 0 then total_net // total_gross else 0.
  in
  let capture =
    if total_gross > 0
    then sprintf "%.1f%%" (capture_ratio *. 100.)
    else "n/a"
  in
  let title =
    sprintf
      "Results — %s · %s · %s"
      (String.concat ~sep:" " (List.map replay.symbols ~f:Symbol.to_string))
      (Date.to_string replay.date)
      (String.uppercase replay.algo_name)
  in
  (* The layman's scoreboard: one verdict sentence, then four big tiles with
     the technical term demoted to a footnote. *)
  let summary =
    let value_add = replay.results.total_value_add_cents in
    let shortfall_bps =
      (run_record replay).History.Run_record.shortfall_bps
    in
    (* The headline is execution quality, not profit: P&L mostly measures
       whether the alpha was right, while slippage against the decision price
       — and against the same orders traded instantly — measures the only
       thing the algorithm controlled. *)
    let verdict =
      let against_benchmark =
        if Float.( < ) shortfall_bps 0.
        then
          sprintf
            "You beat your decision price by %.1f bps"
            (Float.abs shortfall_bps)
        else
          sprintf
            "Execution cost you %.1f bps against your decision price"
            shortfall_bps
      in
      let against_control =
        if value_add > 0
        then
          sprintf
            ", and %s better than trading it all instantly."
            (dollars_signed value_add)
        else if value_add < 0
        then
          sprintf
            ", but %s worse than trading it all instantly."
            (dollars_signed (-value_add))
        else ", matching instant execution exactly."
      in
      against_benchmark ^ against_control
    in
    let money_color cents =
      if cents > 0
      then theme.Styles.green
      else if cents < 0
      then theme.Styles.red
      else theme.Styles.secondary
    in
    (* The mockup's headline band: four cells split by hairlines, the
       plain-words verdict above them, the glossary a click away. *)
    let tile index ~label ~sub ~color value =
      let tile_style =
        Styles.s
          ("display:flex;flex-direction:column;gap:3px;padding:14px 18px;"
           ^
           if index = 0
           then ""
           else "border-left:1px solid " ^ theme.Styles.hairline ^ ";")
      in
      let value_style =
        Styles.s
          ("font-size:22px;font-weight:700;white-space:nowrap;color:"
           ^ color
           ^ ";"
           ^ Styles.mono)
      in
      let sub_style =
        Styles.s
          ("font-size:11px;line-height:1.5;color:" ^ theme.Styles.faint ^ ";")
      in
      {%html|
        <div %{tile_style}>
          <span %{Styles.s (Styles.label theme)}>#{label}</span>
          <span %{value_style}>#{value}</span>
          <span %{sub_style}>#{sub}</span>
        </div>
      |}
    in
    let verdict_style =
      Styles.s
        ("font-size:14.5px;font-weight:600;color:" ^ theme.Styles.text ^ ";")
    in
    let tiles =
      Styles.s
        ("display:grid;grid-template-columns:repeat(auto-fit,minmax(210px,1fr));border-top:1px \
          solid "
         ^ theme.Styles.hairline
         ^ ";")
    in
    let help_button =
      secondary_button
        ~theme
        ~on_click:(fun _ -> open_help)
        "Metrics glossary ?"
    in
    let verdict_row =
      Styles.s
        "display:flex;justify-content:space-between;align-items:flex-start;gap:14px;padding:16px \
         18px;"
    in
    {%html|
      <div %{Styles.card theme ""}>
        <div %{verdict_row}>
          <span %{verdict_style}>#{verdict}</span>
          %{help_button}
        </div>
        <div %{tiles}>
          %{tile 0 ~label:"Your alpha predicted"
              ~sub:"profit if every order filled instantly and free"
              ~color:(money_color total_gross)
              (dollars_signed total_gross)}
          %{tile 1 ~label:"You actually kept"
              ~sub:"net P&L after realistic trading costs"
              ~color:(money_color total_net)
              (dollars_signed total_net)}
          %{tile 2 ~label:"Alpha captured"
              ~sub:(if Float.( > ) capture_ratio 1.
                    then "over 100%: you traded better than the decision price"
                    else "the share of the prediction that survived")
              ~color:(if Float.( > ) capture_ratio 1.
                      then theme.Styles.green else theme.Styles.text)
              capture}
          %{tile 3 ~label:"Execution bonus vs immediate"
              ~sub:"vs selling/buying everything the moment it arrived"
              ~color:(money_color value_add)
              (dollars_signed value_add)}
        </div>
      </div>
    |}
  in
  (* Shared table machinery: css grids with sharp uppercase headers and
     monospace cells sized to survive large dollar figures (12.5px, nowrap,
     roomy columns). *)
  let grid_base columns =
    "display:grid;grid-template-columns:"
    ^ columns
    ^ ";column-gap:10px;align-items:baseline;white-space:nowrap;"
  in
  let head_row columns =
    Styles.s
      (grid_base columns
       ^ "padding:10px 16px 6px 16px;border-bottom:1px solid "
       ^ theme.Styles.hairline
       ^ ";"
       ^ Styles.table_label theme)
  in
  let body_row columns =
    Styles.s
      (grid_base columns
       ^ "padding:8px 16px;font-size:12.5px;color:"
       ^ theme.Styles.text
       ^ ";border-bottom:1px solid "
       ^ theme.Styles.hairline
       ^ ";"
       ^ Styles.mono)
  in
  let total_style columns =
    Styles.s
      (grid_base columns
       ^ "padding:9px 16px;font-size:12.5px;font-weight:700;color:"
       ^ theme.Styles.text
       ^ ";"
       ^ Styles.mono)
  in
  let bold = Styles.s "font-weight:600;" in
  let dim = Styles.s ("color:" ^ theme.Styles.secondary ^ ";") in
  let dash = {%html|<span %{dim}>-</span>|} in
  let blank = {%html|<span></span>|} in
  let order_cell index =
    let chip =
      Styles.s
        ("display:inline-block;width:12px;height:3px;border-radius:2px;vertical-align:middle;background:"
         ^ Styles.order_color theme index
         ^ ";margin-right:8px;")
    in
    {%html|<span %{bold}><span %{chip}></span>O%{index + 1#Int}</span>|}
  in
  let side_qty (grading : Transaction_cost.t) =
    sprintf
      "%s %s"
      (side_str grading.side)
      (Int.to_string_hum ~delimiter:',' (Size.to_int grading.quantity))
  in
  (* Cost table: components in reading order, summing left-to-right into the
     shortfall column. *)
  (* A symbol column only when there is more than one to tell apart. *)
  let multi = List.length replay.symbols > 1 in
  let symbol_cell (grading : Transaction_cost.t) =
    if not multi
    then []
    else [ {%html|<span %{bold}>#{Symbol.to_string grading.symbol}</span>|} ]
  in
  let symbol_head = if multi then [ {%html|<span>symbol</span>|} ] else [] in
  let symbol_blank = if multi then [ blank ] else [] in
  let with_symbol columns = if multi then "68px " ^ columns else columns in
  let cost_columns =
    with_symbol
      "56px minmax(112px,1fr) minmax(104px,1fr) minmax(116px,1fr) \
       minmax(104px,1fr) minmax(104px,1fr) minmax(128px,1.2fr) 1fr"
  in
  let cost_row index (row : Replay.result_row) =
    let grading = row.Replay.grading in
    let avg_fill, shortfall_bps =
      match grading.Transaction_cost.fill_metrics with
      | None -> dash, dash
      | Some metrics ->
        ( {%html|<span>#{sprintf "$%.2f" metrics.average_fill_price}</span>|}
        , bps_view ~theme metrics.shortfall_bps )
    in
    {%html|
      <div %{body_row cost_columns}>
        %{order_cell index}
        *{symbol_cell grading}
        <span %{bold}>#{side_qty grading}</span>
        <span>%{avg_fill}</span>
        <span>%{cost_cell ~theme grading.timing_cost_cents}</span>
        <span>%{cost_cell ~theme grading.spread_cost_cents}</span>
        <span>%{cost_cell ~theme grading.impact_cost_cents}</span>
        <span>%{cost_cell ~theme grading.friction_cost_cents}</span>
        <span>%{shortfall_bps}</span>
      </div>
    |}
  in
  let cost_totals =
    {%html|
      <div %{total_style cost_columns}>
        <span>total</span>
        *{symbol_blank}
        %{blank}
        %{blank}
        <span>%{cost_cell ~theme
            (sum (fun row -> row.Replay.grading.timing_cost_cents))}</span>
        <span>%{cost_cell ~theme
            (sum (fun row -> row.Replay.grading.spread_cost_cents))}</span>
        <span>%{cost_cell ~theme
            (sum (fun row -> row.Replay.grading.impact_cost_cents))}</span>
        <span>%{cost_cell ~theme
            (sum (fun row -> row.Replay.grading.friction_cost_cents))}</span>
        %{blank}
      </div>
    |}
  in
  (* Results table: the P&L identity, net = gross - shortfall - opportunity,
     plus the baseline comparison. *)
  let results_columns =
    with_symbol
      "56px minmax(112px,1fr) 64px minmax(122px,1fr) minmax(118px,1fr) \
       minmax(118px,1fr) minmax(130px,1.1fr) minmax(130px,1.1fr) 1fr"
  in
  let results_row index (row : Replay.result_row) =
    let grading = row.Replay.grading in
    let capture =
      match grading.Transaction_cost.alpha_capture with
      | None -> dash
      | Some capture ->
        {%html|<span %{dim}>#{sprintf "%.1f%%" (capture *. 100.)}</span>|}
    in
    {%html|
      <div %{body_row results_columns}>
        %{order_cell index}
        *{symbol_cell grading}
        <span %{bold}>#{side_qty grading}</span>
        <span %{dim}>
          #{sprintf "%.0f%%" (grading.completion_rate *. 100.)}
        </span>
        <span>%{pnl_cell ~theme grading.gross_theoretical_pnl_cents}</span>
        <span>%{cost_cell ~theme grading.friction_cost_cents}</span>
        <span>%{cost_cell ~theme grading.opportunity_cost_cents}</span>
        <span>%{pnl_cell ~theme grading.net_pnl_cents}</span>
        <span>%{pnl_cell ~theme row.value_add_cents}</span>
        <span>%{capture}</span>
      </div>
    |}
  in
  let results_totals =
    {%html|
      <div %{total_style results_columns}>
        <span>total</span>
        *{symbol_blank}
        %{blank}
        %{blank}
        <span>%{pnl_cell ~theme total_gross}</span>
        <span>%{cost_cell ~theme
            (sum (fun row -> row.Replay.grading.friction_cost_cents))}</span>
        <span>%{cost_cell ~theme
            (sum (fun row -> row.Replay.grading.opportunity_cost_cents))}</span>
        <span>%{pnl_cell ~theme total_net}</span>
        <span>%{pnl_cell ~theme replay.results.total_value_add_cents}</span>
        <span>#{capture}</span>
      </div>
    |}
  in
  let title_style =
    Styles.s
      ("color:" ^ theme.Styles.text ^ ";font-size:14px;font-weight:600;")
  in
  let formula_aside =
    Styles.s
      ("color:" ^ theme.Styles.faint ^ ";font-size:11px;" ^ Styles.mono)
  in
  (* A single-user lab has no one to rank against; what replaces the board is
     the workflow around your own runs. *)
  let actions_card =
    let note =
      Styles.s
        ("color:"
         ^ theme.Styles.faint
         ^ ";font-size:12px;line-height:1.55;margin-top:8px;")
    in
    let compare_hint =
      match previous_run with
      | Some (_ : History.Run_record.t) ->
        "Comparing against your baseline run — the \"What changed?\" panel \
         above shows the deltas."
      | None ->
        "Run another simulation (or pick a baseline in My runs) and the \
         next results will show what changed."
    in
    {%html|
      <div %{Styles.card theme "padding:18px;"}>
        <div
          %{Styles.s
              ("color:"
               ^ theme.Styles.text
               ^ ";font-size:14.5px;font-weight:700;margin-bottom:10px;")}>
          This run is saved
        </div>
        <div %{Styles.s "display:flex;gap:10px;flex-wrap:wrap;"}>
          %{secondary_button ~theme ~on_click:(fun _ -> to_my_runs)
              "View in My runs"}
          %{secondary_button ~theme ~on_click:(fun _ -> retest)
              "Rerun with a different algorithm"}
        </div>
        <div %{note}>#{compare_hint}</div>
      </div>
    |}
  in
  (* The mockup's on-page glossary: every metric's plain-words line in a
     three-column grid; the modal keeps the long-form detail. *)
  let buttons =
    Styles.s "display:flex;gap:10px;align-items:center;flex-wrap:wrap;"
  in
  let export_name suffix =
    sprintf
      "execlab_%s_%s_%s_%s.csv"
      (Replay.symbols_slug replay)
      (Date.to_string replay.date)
      replay.algo_name
      suffix
  in
  let download filename text =
    Effect.of_sync_fun (fun () -> Download.csv ~filename ~text) ()
  in
  let ghost_button ~on_click label =
    let style =
      Styles.s
        ("background:transparent;color:"
         ^ theme.Styles.secondary
         ^ ";border:1px solid "
         ^ theme.Styles.chip_border
         ^ ";border-radius:3px;padding:8px \
            14px;cursor:pointer;font-size:12px;font-weight:600;"
         ^ Styles.mono)
    in
    {%html|<button %{style} on_click=%{on_click}>#{label}</button>|}
  in
  let page =
    Styles.s
      "display:flex;flex-direction:column;gap:20px;max-width:1320px;margin:0 \
       auto;padding:28px 20px;"
  in
  {%html|
    <div class="page fade" %{page}>
      %{wizard_header ~step:4 ?profile ~on_brand ~theme ~is_dark ~toggle_theme ~title
          ~subtitle:"shortfall split into the metric tree: timing + spread \
                     + impact, plus opportunity on unfilled shares"
          ~back:(Some ("← Replay", to_sim)) ()}
      %{summary}
      *{what_changed ~theme ~current:(run_record replay) ~previous:previous_run}
      <div %{Styles.card theme "padding-bottom:4px;overflow-x:auto;"}>
        <div
          %{Styles.s
              "padding:14px 16px 0 16px;display:flex;gap:12px;align-items:baseline;flex-wrap:wrap;"}>
          <span %{title_style}>Execution cost breakdown</span>
          <span %{formula_aside}>timing + spread + impact = shortfall</span>
        </div>
        <div %{head_row cost_columns}>
          <span>order</span>
          *{symbol_head}
          <span>side · qty</span>
          <span>avg fill</span>
          <span>timing</span>
          <span>+ spread</span>
          <span>+ impact</span>
          <span>= shortfall</span>
          <span>in bps</span>
        </div>
        *{List.mapi rows ~f:cost_row}
        %{cost_totals}
      </div>
      <div %{Styles.card theme "padding-bottom:4px;overflow-x:auto;"}>
        <div
          %{Styles.s
              "padding:14px 16px 0 16px;display:flex;gap:12px;align-items:baseline;flex-wrap:wrap;"}>
          <span %{title_style}>P&L identity</span>
          <span %{formula_aside}>gross alpha − shortfall − opportunity = net P&L</span>
        </div>
        <div %{head_row results_columns}>
          <span>order</span>
          *{symbol_head}
          <span>side · qty</span>
          <span>filled</span>
          <span>gross alpha</span>
          <span>shortfall</span>
          <span>opportunity</span>
          <span>net P&L</span>
          <span>vs immediate</span>
          <span>capture</span>
        </div>
        *{List.mapi rows ~f:results_row}
        %{results_totals}
      </div>
      %{actions_card}
      <div %{buttons}>
        %{primary_button ~theme ~on_click:(fun _ -> new_sim)
            "New simulation"}
        %{secondary_button ~theme ~on_click:(fun _ -> to_dashboard)
            "Dashboard"}
        <span
          %{Styles.s "margin-left:auto;display:flex;gap:10px;align-items:center;"}>
          %{ghost_button
              ~on_click:(fun _ ->
                download (export_name "results") (Replay.results_csv replay))
              "↓ Results CSV"}
          %{ghost_button
              ~on_click:(fun _ ->
                download (export_name "fills") (Replay.fills_csv replay))
              "↓ Fills CSV"}
        </span>
      </div>
    </div>
  |}
;;

(* Every dial on the setup screen, explained with concrete numbers, behind
   the (?) in the parameters card's corner. *)
let friction_modal ~theme ~close =
  let backdrop =
    Styles.s
      "position:fixed;inset:0;background:rgba(8,12,20,0.55);display:flex;align-items:flex-start;justify-content:center;padding:40px \
       20px;z-index:50;overflow:auto;"
  in
  let panel =
    Styles.s
      ("background:"
       ^ theme.Styles.card_bg
       ^ ";border:"
       ^ theme.Styles.border
       ^ ";border-radius:6px;max-width:880px;width:100%;padding:24px;"
       ^ theme.Styles.shadow)
  in
  let head =
    Styles.s
      "display:flex;justify-content:space-between;align-items:flex-start;gap:16px;margin-bottom:6px;"
  in
  let title_style =
    Styles.s
      ("color:" ^ theme.Styles.text ^ ";font-size:19px;font-weight:800;")
  in
  let section title =
    {%html|
      <div
        %{Styles.s
            (Styles.label theme
             ^ "margin:18px 0 6px;border-top:1px solid "
             ^ theme.Styles.hairline
             ^ ";padding-top:14px;")}>
        #{title}
      </div>
    |}
  in
  let entry ~term ~text =
    let term_style =
      Styles.s
        ("color:"
         ^ theme.Styles.blue
         ^ ";font-size:12.5px;font-weight:700;"
         ^ Styles.mono)
    in
    let text_style =
      Styles.s
        ("color:"
         ^ theme.Styles.secondary
         ^ ";font-size:13px;line-height:1.65;")
    in
    {%html|
      <div %{Styles.s "margin-bottom:9px;"}>
        <div %{term_style}>#{term}</div>
        <div %{text_style}>#{text}</div>
      </div>
    |}
  in
  {%html|
    <div %{backdrop} on_click=%{fun _ -> close}>
      <div class="fade" %{panel} on_click=%{fun (_ : _) -> Effect.Ignore}>
        <div %{head}>
          <span %{title_style}>What every setting does</span>
          %{secondary_button ~theme ~on_click:(fun _ -> close) "Close"}
        </div>
        %{section "your algorithm's dials — how the strategy trades"}
        %{entry ~term:"pace presets (Passive / Balanced / Aggressive)"
            ~text:"One-click fills for the two dials below. Passive trades \
                   slowly and hides in the market's volume; Aggressive \
                   front-loads and finishes early; Custom means you set \
                   the numbers yourself."}
        %{entry ~term:"participation rate (POV only)"
            ~text:"What share of the market's actual trading POV tries to \
                   be. 0.0015 means: for every 10,000 shares the market \
                   trades in a minute, buy about 15. Higher finishes \
                   sooner but moves the price more. TWAP and VWAP ignore \
                   this — they follow their own schedules."}
        %{entry ~term:"urgency (IS only)"
            ~text:"How much IS front-loads. 0 makes it identical to TWAP \
                   (even pace); higher values do more of the order early, \
                   accepting extra impact now to cut the risk of the \
                   price drifting away later. 1 is mild, 5 is heavy."}
        %{section "market friction — the market you trade against"}
        %{entry ~term:"half spread ($0.02)"
            ~text:"The gap between what buyers bid and sellers ask, \
                   halved. Every time you demand an immediate fill you \
                   pay this per share — 2 cents on every share, in every \
                   aggressive order, before anything else goes wrong. It \
                   is why trading 10,000 shares instantly starts $200 \
                   behind."}
        %{entry ~term:"participation cap (0.10)"
            ~text:"A liquidity limit: one order can consume at most this \
                   fraction of a minute's traded volume — 0.10 means 10%. \
                   If a minute trades 50,000 shares, your order fills at \
                   most 5,000 in it; the rest must wait for later \
                   minutes. This is what stops a huge order filling all \
                   at once."}
        %{entry ~term:"impact coefficient ($0.25)"
            ~text:"How hard your own trading pushes the price away from \
                   you. Taking the full 10% cap in one minute moves your \
                   fill price about 25 cents against you; taking a \
                   quarter of the cap costs about half that (it scales \
                   with the square root of your share). Small orders \
                   barely feel it; big fast orders pay it in full."}
        %{section "fill engine — how fills are decided"}
        %{entry ~term:"bar model"
            ~text:"A formula: each fill is priced from that minute's \
                   real bar — its opening price, plus the half spread, \
                   plus the impact penalty above. Deterministic and \
                   instant; the same run always fills identically."}
        %{entry ~term:"synthetic exchange"
            ~text:"A real (simulated) limit order book instead of a \
                   formula. Background traders keep posting bids and \
                   offers around the day's actual price path, and your \
                   orders match against them in price-time priority — so \
                   queueing, partial fills and impact emerge from the \
                   matching itself. Slower, and random — but seeded, so \
                   the same seed replays the same fills."}
      </div>
    </div>
  |}
;;

(* ---------- landing page ---------- *)

(* A section opener in the mockup's voice: a rule, then an amber small-caps
   kicker. *)
let landing_section_head ~theme text =
  {%html|
    <div
      %{Styles.s
          ("border-top:1px solid "
           ^ theme.Styles.text
           ^ ";padding-top:14px;")}>
      <span %{Styles.s (Styles.kicker theme)}>#{text}</span>
    </div>
  |}
;;

let landing_how_it_works ~theme =
  let steps =
    [ ( "01"
      , "Pick a market day"
      , sprintf
          "A real session — %d symbols, 1-minute closes, 09:30–15:59."
          (List.length Dataset.symbols) )
    ; ( "02"
      , "Paste alpha instructions"
      , "Timestamped parent orders as CSV — or start from a sample." )
    ; ( "03"
      , "Configure execution"
      , "Algorithm, posture, market friction, and the fill engine." )
    ; ( "04"
      , "Replay the day"
      , "Minute-by-minute fills against the tape, at 1×–16×." )
    ; ( "05"
      , "Read the decomposition"
      , "timing + spread + impact = shortfall, plus opportunity cost." )
    ]
  in
  let step index (number, title, blurb) =
    let style =
      Styles.s
        ("display:flex;flex-direction:column;gap:5px;padding:4px 18px 4px 0;"
         ^
         if index = 0
         then ""
         else
           "border-left:1px solid "
           ^ theme.Styles.hairline
           ^ ";padding-left:18px;")
    in
    let number_style =
      Styles.s
        ("color:"
         ^ theme.Styles.brown
         ^ ";font-size:12px;font-weight:700;"
         ^ Styles.mono)
    in
    let title_style =
      Styles.s
        ("color:" ^ theme.Styles.text ^ ";font-size:13.5px;font-weight:700;")
    in
    let blurb_style =
      Styles.s
        ("color:"
         ^ theme.Styles.secondary
         ^ ";font-size:12px;line-height:1.55;")
    in
    {%html|
      <div %{style}>
        <span %{number_style}>#{number}</span>
        <span %{title_style}>#{title}</span>
        <span %{blurb_style}>#{blurb}</span>
      </div>
    |}
  in
  {%html|
    <div
      %{Styles.s
          "display:grid;grid-template-columns:repeat(auto-fit,minmax(190px,1fr));gap:14px;margin-top:16px;"}>
      *{List.mapi steps ~f:step}
    </div>
  |}
;;

(* The algorithm reference data rendered as the mockup's comparison table;
   the heading counts the list rather than naming a number that goes stale
   the next time one is added. *)
let landing_algo_table ~theme =
  let columns = "110px 1.1fr 1.3fr 1.3fr" in
  let head =
    Styles.s
      ("display:grid;grid-template-columns:"
       ^ columns
       ^ ";column-gap:18px;padding:10px 0 8px;border-bottom:1px solid "
       ^ theme.Styles.hairline
       ^ ";")
  in
  let head_cell ?color text =
    let style =
      Styles.s
        (Styles.table_label theme
         ^ match color with None -> "" | Some c -> "color:" ^ c ^ ";")
    in
    {%html|<span %{style}>#{text}</span>|}
  in
  let row ((_ : string), name, tagline, best, weak, (_ : string)) =
    let style =
      Styles.s
        ("display:grid;grid-template-columns:"
         ^ columns
         ^ ";column-gap:18px;padding:11px 0;border-bottom:1px solid "
         ^ theme.Styles.hairline
         ^ ";align-items:baseline;")
    in
    let name_style =
      Styles.s
        ("color:"
         ^ theme.Styles.text
         ^ ";font-size:13px;font-weight:700;"
         ^ Styles.mono)
    in
    let cell_style =
      Styles.s
        ("color:"
         ^ theme.Styles.secondary
         ^ ";font-size:12.5px;line-height:1.55;")
    in
    let join items = String.concat ~sep:"; " items in
    {%html|
      <div %{style}>
        <span %{name_style}>#{name}</span>
        <span %{cell_style}>#{tagline}</span>
        <span %{cell_style}>#{join best}</span>
        <span %{cell_style}>#{join weak}</span>
      </div>
    |}
  in
  let title_row =
    Styles.s
      "display:flex;justify-content:space-between;align-items:baseline;gap:12px;flex-wrap:wrap;margin:14px \
       0 2px;"
  in
  let title_style =
    Styles.s
      ("color:"
       ^ theme.Styles.text
       ^ ";font-size:19px;font-weight:700;"
       ^ Styles.serif)
  in
  {%html|
    <div>
      <div %{title_row}>
        <span %{title_style}>#{in_words (List.length algorithm_cards)} ways
          to work an order</span>
      </div>
      <div %{Styles.s "overflow-x:auto;"}>
        <div %{Styles.s "min-width:760px;"}>
          <div %{head}>
            %{head_cell "algorithm"}
            %{head_cell "schedule"}
            %{head_cell ~color:theme.Styles.green "best when"}
            %{head_cell ~color:theme.Styles.red "weak when"}
          </div>
          *{List.map algorithm_cards ~f:row}
        </div>
      </div>
    </div>
  |}
;;

let landing_engines_and_metrics ~theme =
  let h2 =
    Styles.s
      ("color:"
       ^ theme.Styles.text
       ^ ";font-size:16px;font-weight:700;padding-bottom:8px;border-bottom:2px \
          solid "
       ^ theme.Styles.text
       ^ ";margin-bottom:12px;"
       ^ Styles.serif)
  in
  let term =
    Styles.s
      ("color:"
       ^ theme.Styles.text
       ^ ";font-size:12.5px;font-weight:700;"
       ^ Styles.mono)
  in
  let body =
    Styles.s
      ("color:"
       ^ theme.Styles.secondary
       ^ ";font-size:12.5px;line-height:1.65;")
  in
  let formula =
    let part color text =
      let style =
        Styles.s ("color:" ^ color ^ ";font-weight:700;" ^ Styles.mono)
      in
      {%html|<span %{style}>#{text}</span>|}
    in
    let plus =
      let style = Styles.s ("color:" ^ theme.Styles.faint ^ ";") in
      fun text -> {%html|<span %{style}>#{text}</span>|}
    in
    {%html|
      <div %{Styles.s "display:flex;gap:8px;font-size:13.5px;margin-bottom:10px;flex-wrap:wrap;"}>
        %{part theme.Styles.blue "timing"}
        %{plus "+"}
        %{part theme.Styles.blue "spread"}
        %{plus "+"}
        %{part theme.Styles.blue "impact"}
        %{plus "="}
        %{part theme.Styles.red "shortfall"}
      </div>
    |}
  in
  let stack = Styles.s "display:flex;flex-direction:column;gap:10px;" in
  {%html|
    <div
      %{Styles.s
          "display:grid;grid-template-columns:repeat(auto-fit,minmax(320px,1fr));gap:36px;margin-top:16px;"}>
      <div>
        <div %{h2}>Two fill engines</div>
        <div %{stack}>
          <div>
            <div %{term}>Bar model</div>
            <div %{body}>
              Deterministic fills against historical 1-minute bars, with a
              configurable half-spread and an impact model that grows with
              your share of the minute's volume.
            </div>
          </div>
          <div>
            <div %{term}>Synthetic exchange</div>
            <div %{body}>
              A seeded order book with queue position, partial fills, and
              maker/taker outcomes calibrated to the session.
            </div>
          </div>
        </div>
      </div>
      <div>
        <div %{h2}>What gets measured</div>
        %{formula}
        <div %{body}>
          Every run decomposes implementation shortfall against the arrival
          price, adds opportunity cost on anything left unfilled, and
          reports capture — the share of predicted alpha that survived —
          alongside value added versus an Immediate baseline.
        </div>
      </div>
    </div>
  |}
;;

(* The hero's ambient visual: a real session (TSLA 2026-07-09) drawing itself
   in — price line, VWAP reference, an order window, fills popping as the
   line passes them, and a breathing playhead. Pure CSS motion over real
   data; decorative, but honest decoration. *)
let hero_visual ~theme =
  match
    Dataset.load_cached
      ~symbol:(Symbol.of_string "TSLA")
      ~date:(Date.of_string "2026-07-09")
  with
  | Error (_ : Error.t) -> Vdom.Node.none
  | Ok day ->
    let bars = Array.of_list day.Trading_day.bars in
    let n = Array.length bars in
    let w = 560. in
    let h = 340. in
    let pad = 14. in
    let closes =
      Array.map bars ~f:(fun bar -> Price.to_float bar.Market_bar.close)
    in
    let lo = Array.fold closes ~init:Float.infinity ~f:Float.min in
    let hi = Array.fold closes ~init:Float.neg_infinity ~f:Float.max in
    let span = Float.max (hi -. lo) 0.01 in
    let x i =
      pad +. (Float.of_int i /. Float.of_int (n - 1) *. (w -. (2. *. pad)))
    in
    let y v = pad +. ((hi -. v) /. span *. (h -. (2. *. pad) -. 20.)) in
    let svg name attrs children =
      Vdom.Node.create_svg name ~attrs children
    in
    let attr = Vdom.Attr.create in
    (* Every 4th minute keeps the path light without visibly coarsening. *)
    let sampled =
      List.filter (List.init n ~f:Fn.id) ~f:(fun i -> i % 4 = 0 || i = n - 1)
    in
    let pts =
      List.map sampled ~f:(fun i -> sprintf "%.1f,%.1f" (x i) (y closes.(i)))
    in
    let path = "M " ^ String.concat ~sep:" L " pts in
    let area =
      sprintf
        "%s L %.1f,%.1f L %.1f,%.1f Z"
        path
        (x (n - 1))
        (h -. pad)
        (x 0)
        (h -. pad)
    in
    let vwap = Day_stats.vwap day in
    (* One order window, tinted in the first order color. *)
    let win0 = x 30
    and win1 = x 150 in
    let draw_seconds = 3.8 in
    let start_delay = 0.4 in
    let fill_dot k i =
      let frac = Float.of_int i /. Float.of_int (n - 1) in
      let delay = start_delay +. (draw_seconds *. frac) in
      let color = Styles.order_color theme (k % 2) in
      svg
        "circle"
        [ attr "cx" (sprintf "%.1f" (x i))
        ; attr "cy" (sprintf "%.1f" (y closes.(i)))
        ; attr "r" "3.4"
        ; attr "fill" color
        ; attr "stroke" theme.Styles.card_bg
        ; attr "stroke-width" "1"
        ; Vdom.Attr.class_ "hero-dot"
        ; Styles.s (sprintf "animation-delay:%.2fs;" delay)
        ]
        []
    in
    let dots =
      List.mapi [ 36; 56; 76; 96; 116; 140; 210; 260; 320 ] ~f:fill_dot
    in
    let playhead =
      svg
        "circle"
        [ attr "cx" (sprintf "%.1f" (x (n - 1)))
        ; attr "cy" (sprintf "%.1f" (y closes.(n - 1)))
        ; attr "r" "4.5"
        ; attr "fill" theme.Styles.blue
        ; Vdom.Attr.class_ "hero-pulse"
        ]
        []
    in
    let caption =
      svg
        "text"
        [ attr "x" (sprintf "%.1f" pad)
        ; attr "y" (sprintf "%.1f" (h -. 6.))
        ; attr "fill" theme.Styles.faint
        ; attr "font-size" "10.5"
        ; attr "letter-spacing" "0.08em"
        ]
        [ Vdom.Node.text "TSLA · 2026-07-09 · REPLAYED" ]
    in
    svg
      "svg"
      [ attr "viewBox" (sprintf "0 0 %.0f %.0f" w h)
      ; Styles.s "width:100%;max-width:600px;display:block;"
      ]
      ([ svg
           "rect"
           [ attr "x" (sprintf "%.1f" win0)
           ; attr "y" (sprintf "%.1f" pad)
           ; attr "width" (sprintf "%.1f" (win1 -. win0))
           ; attr "height" (sprintf "%.1f" (h -. (2. *. pad) -. 20.))
           ; attr "fill" (Styles.order_color theme 0)
           ; attr "fill-opacity" "0.07"
           ; Vdom.Attr.class_ "hero-area"
           ]
           []
       ; svg
           "line"
           [ attr "x1" (sprintf "%.1f" pad)
           ; attr "x2" (sprintf "%.1f" (w -. pad))
           ; attr "y1" (sprintf "%.1f" (y vwap))
           ; attr "y2" (sprintf "%.1f" (y vwap))
           ; attr "stroke" theme.Styles.orange
           ; attr "stroke-width" "1.2"
           ; attr "stroke-dasharray" "5 4"
           ; Vdom.Attr.class_ "hero-area"
           ]
           []
       ; svg
           "path"
           [ attr "d" area
           ; attr "fill" theme.Styles.blue
           ; attr "fill-opacity" "0.06"
           ; Vdom.Attr.class_ "hero-area"
           ]
           []
       ; svg
           "path"
           [ attr "d" path
           ; attr "pathLength" "1"
           ; attr "fill" "none"
           ; attr "stroke" theme.Styles.blue
           ; attr "stroke-width" "2"
           ; attr "stroke-linejoin" "round"
           ; Vdom.Attr.class_ "hero-line"
           ]
           []
       ]
       @ dots
       @ [ playhead; caption ])
;;

let landing_view ~theme ~is_dark ~toggle_theme ~enter ~to_dashboard =
  let page =
    Styles.s
      "display:flex;flex-direction:column;gap:34px;max-width:1240px;margin:0 \
       auto;padding:26px 24px 60px;"
  in
  let bar =
    Styles.s
      ("display:flex;justify-content:space-between;align-items:center;gap:12px;flex-wrap:wrap;padding-bottom:12px;border-bottom:2px \
        solid "
       ^ theme.Styles.text
       ^ ";")
  in
  let hero_kicker =
    {%html|<div %{Styles.s (Styles.kicker theme)}>your local execution lab</div>|}
  in
  (* Plain words: what it is, what you do with it. *)
  let headline =
    let style =
      Styles.s
        ("color:"
         ^ theme.Styles.text
         ^ ";font-size:clamp(30px,3.8vw,44px);font-weight:700;line-height:1.14;margin:8px \
            0 0;letter-spacing:-0.01em;max-width:17ch;"
         ^ Styles.serif)
    in
    {%html|
      <h1 %{style}>
        Run your trades through a real market day and see what they cost.
      </h1>
    |}
  in
  let subhead =
    let style =
      Styles.s
        ("color:"
         ^ theme.Styles.secondary
         ^ ";font-size:14px;line-height:1.7;max-width:56ch;")
    in
    {%html|
      <div %{style}>
        Bring a list of orders — or start from a sample — pick a historical
        trading day and an execution algorithm, and replay the session
        minute by minute. Every run is graded and saved on this machine so
        you can compare your experiments over time.
      </div>
    |}
  in
  let cta =
    {%html|
      <div
        %{Styles.s
            "display:flex;gap:14px;align-items:center;flex-wrap:wrap;margin-top:18px;"}>
        %{primary_button ~icon:(Icon.arrow_right ~size:14 ()) ~theme
            ~on_click:(fun _ -> enter) "Run simulation"}
        %{secondary_button ~theme ~on_click:(fun _ -> to_dashboard)
            "My dashboard"}
      </div>
    |}
  in
  {%html|
    <div class="page fade" %{page}>
      <div %{bar}>
        %{wordmark ~theme ()}
        <span %{Styles.s "display:flex;gap:10px;align-items:center;"}>
          %{theme_button ~theme ~is_dark ~toggle_theme}
        </span>
      </div>
      <div
        %{Styles.s
            "display:grid;grid-template-columns:repeat(auto-fit,minmax(340px,1fr));gap:40px;align-items:center;"}>
        <div>
          %{hero_kicker}
          %{headline}
          <div %{Styles.s "margin-top:14px;"}>%{subhead}</div>
          %{cta}
        </div>
        <div %{Styles.s "display:flex;justify-content:flex-end;"}>
          %{hero_visual ~theme}
        </div>
      </div>
      <div>
        %{landing_section_head ~theme "how a run works"}
        %{landing_how_it_works ~theme}
      </div>
      %{landing_algo_table ~theme}
      %{landing_engines_and_metrics ~theme}
    </div>
  |}
;;

(* ---------- the app ---------- *)

(* The overscroll area above/below the app shows the document's own
   background, so theme flips must recolor <html>/<body> too. *)
let set_page_background =
  let set color =
    let open Js_of_ocaml in
    let background = Js.string color in
    Dom_html.document##.documentElement##.style##.background := background;
    Dom_html.document##.body##.style##.background := background
  in
  Effect.of_sync_fun set
;;

(* One dashboard row per completed run, aggregated across its orders. *)
let app (local_ graph) : Vdom.Node.t Bonsai.t =
  let screen, set_screen = Bonsai.state Screen.Landing graph in
  let show_help, set_show_help = Bonsai.state false graph in
  let show_friction_help, set_show_friction_help =
    Bonsai.state false graph
  in
  let selection, set_selection = Bonsai.state (None : Date.t option) graph in
  let alpha_text, set_alpha_text =
    Bonsai.state Embedded_data.demo_alpha graph
  in
  let algo, set_algo = Bonsai.state "twap" graph in
  let run_error, set_run_error =
    Bonsai.state (None : Error.t option) graph
  in
  let runs, set_runs = Bonsai.state (History.load ()) graph in
  (* The run that was on top of the notebook when this one started — the
     baseline the results screen diffs against. *)
  (* [`Auto r] follows the freshest run; [`Pinned r] was chosen in My runs
     and survives new runs until deleted or unpinned. *)
  let baseline, set_baseline =
    Bonsai.state (`Auto (None : History.Run_record.t option)) graph
  in
  let confirm_clear, set_confirm_clear = Bonsai.state false graph in
  let replay, set_replay = Bonsai.state (None : Replay.t option) graph in
  let minute, set_minute = Bonsai.state' 0 graph in
  let playing, set_playing = Bonsai.state true graph in
  let speed, set_speed = Bonsai.state 4 graph in
  let show_fills, set_show_fills = Bonsai.state true graph in
  (* Which name the chart is drawing. Held as an option and resolved against
     the run in hand, so a stale pick from the last alpha can never point at
     a symbol this run never traded. *)
  let focus, set_focus = Bonsai.state (None : Symbol.t option) graph in
  let param_text, set_param_text =
    Bonsai.state Replay.Param_text.default graph
  in
  let hover, set_hover = Bonsai.state (None : int option) graph in
  let chart_view, set_chart_view =
    Bonsai.state (Chart_view.Follow None) graph
  in
  let zoom_mode, set_zoom_mode = Bonsai.state false graph in
  let zoom_tool, set_zoom_tool =
    Bonsai.state (None : [ `In | `Out ] option) graph
  in
  (* While panning: the cursor's plot ratio at mousedown and the window start
     it was anchored to. *)
  let drag, set_drag = Bonsai.state (None : (float * int) option) graph in
  let is_dark, set_is_dark =
    Bonsai.state
      (match Storage.get Storage.theme_key with
       | Some "dark" -> true
       | Some (_ : string) | None -> false)
      graph
  in
  let advance =
    let%arr playing and replay and set_minute in
    match replay with
    | Some r when playing ->
      set_minute (fun m -> Int.min (Replay.last_minute r) (m + 1))
    | Some _ | None -> Effect.Ignore
  in
  (* Smoothness comes from the tick, not the step: the playhead always moves
     one minute at a time, and speeding up shortens the tick instead of
     lengthening the jump. At 16x that is a tick every ~16ms — animation rate
     — where the old +16-minutes-per-quarter-second visibly lurched. *)
  let tick_span =
    let%arr speed in
    Time_ns.Span.of_ms (250. /. Float.of_int (Int.max 1 speed))
  in
  Bonsai.Clock.every
    ~when_to_start_next_effect:`Every_multiple_of_period_non_blocking
    ~trigger_on_activate:false
    tick_span
    advance
    graph;
  let start =
    let%arr algo
    and selection
    and alpha_text
    and param_text
    and runs
    and set_runs
    and set_run_error
    and set_replay
    and set_screen
    and set_minute
    and set_playing
    and baseline
    and set_baseline in
    match selection, Replay.parse_params param_text with
    | None, _ -> set_run_error (Some (Error.of_string "choose a day first"))
    | Some (_ : Date.t), Error error -> set_run_error (Some error)
    | Some date, Ok params ->
      let%bind.Effect result =
        Effect.of_sync_fun
          (fun () -> Replay.run ~date ~alpha_text ~algo_name:algo ~params)
          ()
      in
      (match result with
       | Error error -> set_run_error (Some error)
       | Ok r ->
         let%bind.Effect () = set_run_error None in
         let%bind.Effect () = set_replay (Some r) in
         let%bind.Effect () = set_minute (fun (_ : int) -> 0) in
         let%bind.Effect () = set_playing true in
         (* The freshest previous run becomes the comparison baseline —
            unless one was pinned in My runs, which a new run never
            overrides. *)
         let%bind.Effect () =
           match baseline with
           | `Pinned (_ : History.Run_record.t) -> Effect.Ignore
           | `Auto (_ : History.Run_record.t option) ->
             set_baseline (`Auto (List.hd runs))
         in
         let%bind.Effect () = set_runs (History.add (run_record r) runs) in
         set_screen Screen.Sim)
  in
  let restart =
    let%arr set_minute and set_playing in
    let%bind.Effect () = set_minute (fun (_ : int) -> 0) in
    set_playing true
  in
  (* Local run actions: everything runs off the history record, which carries
     the complete config. *)
  let open_run =
    let%arr set_replay
    and set_run_error
    and set_minute
    and set_selection
    and set_alpha_text
    and set_algo
    and set_screen in
    fun (record : History.Run_record.t) ->
      let%bind.Effect result =
        Effect.of_sync_fun (fun () -> replay_of_record record) ()
      in
      match result with
      | Error error -> set_run_error (Some error)
      | Ok r ->
        (* Restore the run's whole context, not just its results: Results
           renders only when a day is selected, and Replay/Setup navigation
           from there must agree with what is on screen. *)
        let%bind.Effect () = set_selection (Some record.date) in
        let%bind.Effect () = set_alpha_text record.alpha_text in
        let%bind.Effect () = set_algo record.algo_name in
        let%bind.Effect () = set_replay (Some r) in
        let%bind.Effect () =
          set_minute (fun (_ : int) -> Replay.last_minute r)
        in
        set_screen Screen.Results
  in
  (* Rerun: load the run's day and instructions into the wizard and land on
     Setup, so changing the algorithm is one click away. *)
  let rerun_run =
    let%arr set_selection
    and set_alpha_text
    and set_algo
    and set_param_text
    and set_screen in
    fun (record : History.Run_record.t) ->
      let%bind.Effect () = set_selection (Some record.date) in
      let%bind.Effect () = set_alpha_text record.alpha_text in
      let%bind.Effect () = set_algo record.algo_name in
      (* The run's own dials come back with it; a rerun that silently
         reverted to defaults would not be a rerun. *)
      let%bind.Effect () =
        set_param_text
          { Replay.Param_text.half_spread =
              sprintf "%.2f" (Float.of_int record.half_spread_cents /. 100.)
          ; participation = sprintf "%.2f" record.max_participation
          ; impact =
              sprintf
                "%.2f"
                (Float.of_int record.impact_coefficient_cents /. 100.)
          ; pov_rate = sprintf "%.4f" record.pov_rate
          ; urgency = sprintf "%.1f" record.is_urgency
          ; patience = sprintf "%.2f" record.patience
          ; engine = record.engine_name
          ; seed = Int.to_string record.engine_seed
          }
      in
      set_screen Screen.Setup
  in
  let set_baseline_run =
    let%arr set_baseline and set_screen in
    fun (record : History.Run_record.t) ->
      let%bind.Effect () = set_baseline (`Pinned record) in
      set_screen Screen.My_runs
  in
  let delete_run =
    let%arr runs and set_runs and baseline and set_baseline in
    fun (record : History.Run_record.t) ->
      let id = History.Run_record.id record in
      (* A deleted run cannot stay the comparison baseline. *)
      let%bind.Effect () =
        match baseline with
        | `Pinned pinned when String.equal (History.Run_record.id pinned) id
          ->
          set_baseline (`Auto None)
        | `Auto (Some auto) when String.equal (History.Run_record.id auto) id
          ->
          set_baseline (`Auto None)
        | `Pinned (_ : History.Run_record.t)
        | `Auto (Some (_ : History.Run_record.t))
        | `Auto None ->
          Effect.Ignore
      in
      set_runs (History.remove ~id runs)
  in
  let clear_history =
    let%arr confirm_clear and set_confirm_clear and set_runs in
    if not confirm_clear
    then set_confirm_clear true
    else (
      let%bind.Effect () =
        Effect.of_sync_fun (fun () -> History.clear ()) ()
      in
      let%bind.Effect () = set_confirm_clear false in
      set_runs [])
  in
  let%arr screen
  and selection
  and set_selection
  and alpha_text
  and set_alpha_text
  and algo
  and set_algo
  and param_text
  and set_param_text
  and run_error
  and runs
  and baseline
  and replay
  and minute
  and playing
  and set_playing
  and speed
  and set_speed
  and show_fills
  and set_show_fills
  and show_help
  and set_show_help
  and show_friction_help
  and set_show_friction_help
  and hover
  and set_hover
  and focus
  and set_focus
  and chart_view
  and set_chart_view
  and zoom_mode
  and set_zoom_mode
  and zoom_tool
  and set_zoom_tool
  and drag
  and set_drag
  and is_dark
  and set_is_dark
  and set_minute
  and set_screen
  and start
  and restart
  and confirm_clear
  and set_confirm_clear
  and open_run
  and rerun_run
  and set_baseline_run
  and delete_run
  and clear_history in
  let theme = if is_dark then Styles.dark else Styles.paper in
  let toggle_theme =
    let next = if is_dark then Styles.paper else Styles.dark in
    let%bind.Effect () = set_is_dark (not is_dark) in
    let%bind.Effect () =
      Effect.of_sync_fun
        (fun () ->
          Storage.set Storage.theme_key (if is_dark then "light" else "dark"))
        ()
    in
    set_page_background next.Styles.page_bg
  in
  let goto s =
    (* Navigating away disarms any pending destructive confirmation. *)
    Effect.Many [ set_confirm_clear false; set_screen s ]
  in
  let select date = set_selection (Some date) in
  let on_brand = goto Screen.Landing in
  let open_dashboard = goto Screen.Dashboard in
  let open_my_runs = goto Screen.My_runs in
  (* Always visible, always the same place: the way to your own work. *)
  let profile = Some (my_dashboard_button ~theme ~on_click:open_dashboard) in
  let dashboard () =
    dashboard_view
      ~theme
      ~is_dark
      ~runs
      ~new_sim:(goto Screen.Choose_day)
      ~to_my_runs:open_my_runs
      ~profile
      ~on_brand
      ~toggle_theme
  in
  let choose_day () =
    choose_day_view
      ~theme
      ~is_dark
      ~selection
      ~select
      ~continue_:(goto Screen.Alpha)
      ~profile
      ~on_brand
      ~toggle_theme
      ~back:(goto Screen.Dashboard)
  in
  let body =
    match (screen : Screen.t), replay, selection with
    | Landing, _, _ ->
      landing_view
        ~theme
        ~is_dark
        ~toggle_theme
        ~enter:(goto Screen.Choose_day)
        ~to_dashboard:open_dashboard
    | My_runs, _, _ ->
      my_runs_view
        ~theme
        ~is_dark
        ~toggle_theme
        ~profile
        ~on_brand
        ~runs
        ~run_error
        ~open_run
        ~rerun:rerun_run
        ~set_baseline:set_baseline_run
        ~delete:delete_run
        ~clear_all:clear_history
        ~confirm_clear
        ~new_sim:(goto Screen.Choose_day)
        ~back:open_dashboard
    | Dashboard, _, _ -> dashboard ()
    | Choose_day, _, _ -> choose_day ()
    | (Alpha | Setup | Sim | Results), _, None -> choose_day ()
    | Alpha, _, Some date ->
      alpha_view
        ~theme
        ~is_dark
        ~date
        ~alpha_text
        ~set_alpha_text
        ~continue_:(goto Screen.Setup)
        ~profile
        ~on_brand
        ~toggle_theme
        ~back:(goto Screen.Choose_day)
    | Setup, _, Some date | (Sim | Results), None, Some date ->
      setup_view
        ~theme
        ~is_dark
        ~open_friction_help:(set_show_friction_help true)
        ~date
        ~alpha_text
        ~algo
        ~set_algo
        ~param_text
        ~set_param_text
        ~start
        ~run_error
        ~profile
        ~on_brand
        ~toggle_theme
        ~back:(goto Screen.Alpha)
    | Sim, Some r, Some (_ : Date.t) ->
      sim_view
        r
        ~theme
        ~is_dark
        ~focus:
          (match focus with
           | Some symbol when List.mem r.symbols symbol ~equal:Symbol.equal
             ->
             symbol
           | Some (_ : Symbol.t) | None -> List.hd_exn r.symbols)
        ~set_focus:(fun symbol -> set_focus (Some symbol))
        ~minute
        ~playing
        ~speed
        ~show_fills
        ~chart_view
        ~set_chart_view
        ~zoom_mode
        ~set_zoom_mode
        ~zoom_tool
        ~set_zoom_tool
        ~drag
        ~set_drag
        ~set_playing
        ~set_speed
        ~set_minute
        ~restart
        ~toggle_fills:(set_show_fills (not show_fills))
        ~toggle_theme
        ~to_results:(goto Screen.Results)
        ~back:(goto Screen.Setup)
        ~profile
        ~on_brand
        ~hover
        ~set_hover
    | Results, Some r, Some (_ : Date.t) ->
      results_view
        r
        ~theme
        ~is_dark
        ~open_help:(set_show_help true)
        ~previous_run:
          (match baseline with
           | `Pinned record -> Some record
           | `Auto auto -> auto)
        ~profile
        ~on_brand
        ~to_sim:(goto Screen.Sim)
        ~new_sim:(goto Screen.Choose_day)
        ~to_my_runs:open_my_runs
        ~retest:(goto Screen.Setup)
        ~to_dashboard:open_dashboard
        ~toggle_theme
  in
  let shell =
    Styles.s
      ("min-height:100vh;background:"
       ^ theme.Styles.page_bg
       ^ ";color-scheme:"
       ^ (if is_dark then "dark" else "light")
       ^ ";font-family:system-ui,-apple-system,'Segoe UI',sans-serif;")
  in
  (* Keyed on the screen so vdom *rebuilds* this subtree on navigation
     instead of patching it in place — a patched element keeps running its
     old animation state, which is why only the first screen visibly
     animated. [display:contents] keeps the wrapper out of layout. *)
  let overlay =
    if show_help
    then [ help_modal ~theme ~close:(set_show_help false) ]
    else if show_friction_help
    then [ friction_modal ~theme ~close:(set_show_friction_help false) ]
    else []
  in
  let keyed_body =
    Vdom.Node.div
      ~key:(Sexp.to_string [%sexp (screen : Screen.t)])
      ~attrs:[ Styles.s "display:contents;" ]
      [ body ]
  in
  {%html|<div class="shell-decor" %{shell}>%{keyed_body} *{overlay}</div>|}
;;

let () = Bonsai_web.Start.start app
