open! Core
open! Bonsai_web
open Bonsai.Let_syntax
open! Execlab_types
open! Execlab_market
open! Execlab_analytics
open Execlab_protocol

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

(* The landing card is one form with two exits: signing in and creating an
   account share the handle/passcode fields and differ only in which endpoint
   the clicked button calls. *)
module Auth_mode = struct
  type t =
    | Sign_in
    | Create_account
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

(* A small labelled icon button for secondary table actions. *)
let icon_action ~theme ~glyph ~label ~on_click =
  let style =
    Styles.s
      ("display:inline-flex;align-items:center;gap:6px;background:"
       ^ theme.Styles.chip_bg
       ^ ";color:"
       ^ theme.Styles.secondary
       ^ ";border:1px solid "
       ^ theme.Styles.chip_border
       ^ ";border-radius:7px;padding:6px \
          11px;cursor:pointer;font-size:12px;font-weight:700;")
  in
  {%html|
    <button class="btn" %{style} on_click=%{on_click}>
      %{glyph}
      #{label}
    </button>
  |}
;;

(* The account chip: identity plus a way into your own runs, in the same
   corner on every screen. Guests get a way to sign in instead. *)
let profile_button ~theme ~session ~on_click =
  (* The mockup's header identity: "signed in as qomer", a quiet mono line
     that is also the way into your own runs. Guests get "guest". *)
  let prefix, name =
    match (session : Session.t option) with
    | Some { Session.username; token = (_ : string) } ->
      "signed in as ", username
    | None -> "", "guest"
  in
  let style =
    Styles.s
      ("display:inline-flex;align-items:baseline;gap:5px;background:transparent;color:"
       ^ theme.Styles.faint
       ^ ";border:none;padding:4px \
          2px;cursor:pointer;font-size:11.5px;white-space:nowrap;"
       ^ Styles.mono)
  in
  let name_style =
    Styles.s ("color:" ^ theme.Styles.text ^ ";font-weight:700;")
  in
  {%html|
    <button
      class="btn"
      %{style}
      title="Your runs"
      on_click=%{fun _ -> on_click}>
      #{prefix}
      <span %{name_style}>#{name}</span>
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
       ^ theme.Styles.text
       ^ ";font-size:15.5px;font-weight:800;letter-spacing:-0.01em;")
  in
  let suffix =
    Styles.s
      ("color:"
       ^ theme.Styles.faint
       ^ ";font-size:9.5px;font-weight:600;letter-spacing:0.18em;text-transform:uppercase;"
       ^ Styles.mono)
  in
  let lockup =
    {%html|
      <span %{Styles.s "display:inline-flex;align-items:baseline;gap:9px;white-space:nowrap;"}>
        <span %{name}>ExecLab</span>
        <span %{suffix}>execution research</span>
      </span>
    |}
  in
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
  let refresh ?size () = make ?size Refresh_cw
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
let alpha_symbols_line ~fallback alpha_text =
  match Replay.parse_alpha alpha_text with
  | Error (_ : Error.t) -> Symbol.to_string fallback
  | Ok [] -> Symbol.to_string fallback
  | Ok instructions ->
    List.map instructions ~f:(fun instruction ->
      instruction.Alpha_instruction.symbol)
    |> List.dedup_and_sort ~compare:Symbol.compare
    |> List.map ~f:Symbol.to_string
    |> String.concat ~sep:" "
;;

let symbols_label symbols =
  match symbols with
  | [] -> "-"
  | [ symbol ] -> Symbol.to_string symbol
  | first :: rest ->
    sprintf "%s +%d" (Symbol.to_string first) (List.length rest)
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
                     you paid %+.4f vs the open"
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
           :: List.map covering ~f:(fun ((_ : string), text) ->
             width ~per_char:6.3 text))
          ~init:0.
          ~f:Float.max
        +. 20.
      in
      let line_h = 15. in
      let box_h = 40. +. (Float.of_int (List.length covering) *. line_h) in
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
      @ List.mapi covering ~f:(fun i (color, text) ->
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
    sprintf
      "dune exec bin/main.exe -- <your_alpha.csv> %s %s"
      (Date.to_string replay.date)
      replay.algo_name
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
      <div
        %{Styles.s
            "display:grid;grid-template-columns:minmax(0,2.2fr) minmax(300px,1fr);gap:16px;align-items:start;"}>
        <div %{Styles.s "overflow-x:auto;min-width:0;"}>
          %{orders_table replay ~theme ~fills ~minute}
        </div>
        %{event_log replay ~theme ~fills ~minute}
      </div>
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

let pnl_cell_int63 ~theme cents =
  let dollars = Run_summary.dollars cents in
  let color =
    if Float.( > ) dollars 0.
    then theme.Styles.green
    else if Float.( < ) dollars 0.
    then theme.Styles.red
    else theme.Styles.faint
  in
  let style =
    Styles.s
      ("color:" ^ color ^ ";font-size:13px;font-weight:600;" ^ Styles.mono)
  in
  let text =
    sprintf
      "%s$%s"
      (if Float.( < ) dollars 0. then "-" else "+")
      (Float.to_string_hum ~delimiter:',' ~decimals:2 (Float.abs dollars))
  in
  {%html|<span %{style}>#{text}</span>|}
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

let my_runs_view
  ~theme
  ~is_dark
  ~profile
  ~on_brand
  ~toggle_theme
  ~session
  ~my_runs
  ~open_run
  ~refresh
  ~reset_account
  ~confirm_reset
  ~new_sim
  ~back
  =
  let columns = "128px 72px 66px 58px 1fr 118px 118px 70px 90px 64px" in
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
  (* The knobs that actually shaped this run, so a notebook entry is
     reproducible by reading it. Only the algorithm's own parameter is shown
     — the fill-model numbers live in the tooltip. *)
  let params_of (config : Run_config.t) =
    match config.algo_name with
    | "pov" -> sprintf "rate %.4f" config.pov_rate
    | "is" -> sprintf "urgency %.1f" config.is_urgency
    | (_ : string) -> "—"
  in
  let full_params (config : Run_config.t) =
    sprintf
      "half spread $%.2f · participation %.2f · impact $%.2f · pov %.4f · \
       urgency %.1f"
      (Float.of_int config.half_spread_cents /. 100.)
      config.max_participation
      (Float.of_int config.impact_coefficient_cents /. 100.)
      config.pov_rate
      config.is_urgency
  in
  let run_row (run : Saved_run.t) =
    let style =
      Styles.s
        ("display:grid;grid-template-columns:"
         ^ columns
         ^ ";column-gap:12px;white-space:nowrap;align-items:center;padding:9px \
            16px;font-size:12px;color:"
         ^ theme.Styles.text
         ^ ";border-bottom:1px solid "
         ^ theme.Styles.hairline
         ^ ";"
         ^ Styles.mono)
    in
    let capture =
      match run.summary.alpha_capture with
      | None -> "—"
      | Some capture -> sprintf "%.1f%%" (capture *. 100.)
    in
    let state =
      if run.published
      then (
        let style =
          Styles.s
            ("color:"
             ^ theme.Styles.green
             ^ ";font-weight:600;font-size:11.5px;")
        in
        {%html|<span %{style}>published</span>|})
      else (
        let style =
          Styles.s ("color:" ^ theme.Styles.faint ^ ";font-size:11.5px;")
        in
        {%html|<span %{style}>private</span>|})
    in
    let open_link =
      let style =
        Styles.s
          ("background:none;border:none;padding:2px 0;color:"
           ^ theme.Styles.blue
           ^ ";cursor:pointer;font-size:12px;font-weight:600;text-align:left;"
           ^ Styles.mono)
      in
      {%html|
        <button class="btn" %{style} on_click=%{fun _ -> open_run run}>
          open →
        </button>
      |}
    in
    let saved_at =
      String.map (String.prefix run.ran_at 16) ~f:(fun c ->
        if Char.equal c 'T' then ' ' else c)
    in
    let engine =
      match run.config.engine_name with
      | "synthetic" -> "synth"
      | (_ : string) -> "bar"
    in
    {%html|
      <div %{style} title=%{full_params run.config}>
        <span %{dim}>#{saved_at}</span>
        <span>#{symbols_label run.config.symbols}</span>
        <span>#{String.uppercase run.config.algo_name}</span>
        <span %{dim}>#{engine}</span>
        <span %{dim}>
          #{Date.to_string run.config.date} · #{params_of run.config}
        </span>
        <span>%{pnl_cell_int63 ~theme run.summary.net_cents}</span>
        <span>%{pnl_cell_int63 ~theme run.summary.value_add_cents}</span>
        <span %{dim}>#{capture}</span>
        %{state}
        %{open_link}
      </div>
    |}
  in
  let body =
    match (my_runs : Saved_run.t list option) with
    | None ->
      [ {%html|
          <div %{Styles.s "padding:16px;"}>
            <span %{faint_style}>Loading your runs…</span>
          </div>
        |}
      ]
    | Some [] ->
      [ {%html|
          <div
            %{Styles.s
                "padding:26px 16px;display:flex;flex-direction:column;gap:12px;align-items:flex-start;"}>
            <span %{faint_style}>
              No runs yet. Every simulation you execute while signed in is
              recorded here automatically.
            </span>
            %{primary_button ~icon:(Icon.arrow_right ~size:15 ()) ~theme
                ~on_click:(fun _ -> new_sim) "Run your first simulation"}
          </div>
        |}
      ]
    | Some runs ->
      {%html|
        <div %{head}>
          <span>saved</span>
          <span>sym</span>
          <span>algo</span>
          <span>engine</span>
          <span>day · settings</span>
          <span>net P&L</span>
          <span>vs immediate</span>
          <span>capture</span>
          <span>state</span>
          <span>actions</span>
        </div>
      |}
      :: List.map runs ~f:run_row
  in
  (* The mockup's slim stats line: "14 saved | 5 published | 6 symbols | best
     vs Immediate +$1,072.25". *)
  let stats_line =
    match (my_runs : Saved_run.t list option) with
    | None | Some [] -> []
    | Some runs ->
      let published =
        List.count runs ~f:(fun (run : Saved_run.t) -> run.published)
      in
      let symbols =
        List.concat_map runs ~f:(fun (run : Saved_run.t) ->
          run.config.symbols)
        |> List.dedup_and_sort ~compare:Symbol.compare
        |> List.length
      in
      let best =
        List.map runs ~f:(fun (run : Saved_run.t) ->
          run.summary.value_add_cents)
        |> List.max_elt ~compare:Int63.compare
      in
      let sep =
        Styles.s
          ("color:"
           ^ theme.Styles.hairline
           ^ ";align-self:stretch;border-left:1px solid "
           ^ theme.Styles.hairline
           ^ ";")
      in
      let piece text =
        let style =
          Styles.s
            ("color:"
             ^ theme.Styles.secondary
             ^ ";font-size:12px;"
             ^ Styles.mono)
        in
        {%html|<span %{style}>#{text}</span>|}
      in
      let best_piece =
        match best with
        | None -> []
        | Some v ->
          [ {%html|<span %{sep}></span>|}
          ; {%html|
              <span
                %{Styles.s
                    ("color:"
                     ^ theme.Styles.secondary
                     ^ ";font-size:12px;display:inline-flex;gap:6px;align-items:baseline;"
                     ^ Styles.mono)}>
                best vs Immediate %{pnl_cell_int63 ~theme v}
              </span>
            |}
          ]
      in
      [ {%html|
          <div
            %{Styles.s
                ("display:flex;gap:14px;align-items:baseline;padding:10px 16px;border-bottom:1px solid "
                 ^ theme.Styles.hairline
                 ^ ";")}>
            %{piece (sprintf "%d saved" (List.length runs))}
            <span %{sep}></span>
            %{piece (sprintf "%d published" published)}
            <span %{sep}></span>
            %{piece (sprintf "%d symbols" symbols)}
            *{best_piece}
          </div>
        |}
      ]
  in
  (* Destructive, so it asks twice: the first click arms it and turns it red,
     the second actually erases. *)
  let reset_button =
    let style =
      Styles.s
        ("display:inline-flex;align-items:center;gap:6px;background:"
         ^ (if confirm_reset then theme.Styles.red else "transparent")
         ^ ";color:"
         ^ (if confirm_reset then "#ffffff" else theme.Styles.red)
         ^ ";border:1px solid "
         ^ (if confirm_reset
            then theme.Styles.red
            else theme.Styles.chip_border)
         ^ ";border-radius:3px;padding:6px \
            11px;cursor:pointer;font-size:12px;font-weight:700;")
    in
    {%html|
      <button class="btn" %{style} on_click=%{fun _ -> reset_account}>
        #{if confirm_reset
          then "Click again to erase everything"
          else "Reset my data"}
      </button>
    |}
  in
  let who =
    match (session : Session.t option) with
    | None -> "not signed in"
    | Some { Session.username; token = (_ : string) } ->
      sprintf "signed in as %s" username
  in
  let footer_note =
    Styles.s
      ("color:"
       ^ theme.Styles.faint
       ^ ";font-size:11.5px;line-height:1.6;padding:10px 16px 14px;")
  in
  {%html|
    <div class="page fade" %{Styles.s narrow_page}>
      %{wizard_header ?profile ~on_brand ~theme ~is_dark ~toggle_theme ~title:"My runs"
          ~subtitle:("your server-side research notebook — every saved run, \
                      reopenable and reproducible — " ^ who)
          ~back:(Some ("← Dashboard", back)) ()}
      <div %{Styles.card theme "padding-bottom:4px;"}>
        <div
          %{Styles.s
              "display:flex;justify-content:space-between;align-items:center;padding:12px 16px;"}>
          <span
            %{Styles.s
                ("color:"
                 ^ theme.Styles.text
                 ^ ";font-size:14.5px;font-weight:700;")}>
            Saved runs
          </span>
          <span %{Styles.s "display:flex;gap:8px;align-items:center;"}>
            %{icon_action ~theme ~glyph:(Icon.refresh ~size:14 ())
                ~label:"Refresh" ~on_click:(fun _ -> refresh)}
            %{reset_button}
          </span>
        </div>
        *{stats_line}
        <div %{Styles.s "overflow-x:auto;"}>
          <div %{Styles.s "min-width:980px;"}>
            *{body}
          </div>
        </div>
        <div %{footer_note}>
          Published runs appear on the day's leaderboard after server-side
          re-scoring under house physics. Reopening a run restores its exact
          configuration and results.
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
        *{List.map glossary_entries ~f:entry}
      </div>
    </div>
  |}
;;

let dashboard_view
  ~theme
  ~is_dark
  ~runs
  ~new_sim
  ~session
  ~sign_out
  ~to_sign_in
  ~to_my_runs
  ~quick_start_with
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
  (* Sign out lives in the header, next to the identity it acts on; guests
     get the way in instead. *)
  let session_link =
    match (session : Session.t option) with
    | Some (_ : Session.t) -> "Sign out", sign_out
    | None -> "Sign in", to_sign_in
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
    let median_capture =
      let captures =
        List.filter_map runs ~f:(fun (run : History.Run_record.t) ->
          run.alpha_capture)
        |> List.sort ~compare:Float.compare
      in
      let n = List.length captures in
      if n = 0
      then None
      else if n % 2 = 1
      then List.nth captures (n / 2)
      else (
        match List.nth captures ((n / 2) - 1), List.nth captures (n / 2) with
        | Some a, Some b -> Some ((a +. b) /. 2.)
        | (_ : float option), (_ : float option) -> None)
    in
    let latest =
      match List.hd runs with
      | None -> "—"
      | Some (run : History.Run_record.t) ->
        sprintf
          "%s · %s · %s"
          (History.Run_record.symbols_label run)
          (Date.to_string run.date)
          (String.uppercase run.algo_name)
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
          %{cell 0 ~label:"saved runs" ~color:theme.Styles.text
              (Int.to_string count)}
          %{cell 1 ~label:"best value-add vs immediate" ~color:best_color
              (match best_value with
               | None -> "—"
               | Some v -> dollars_signed v)}
          %{cell 2 ~label:"median capture" ~color:theme.Styles.text
              (match median_capture with
               | None -> "—"
               | Some c -> sprintf "%.1f%%" (c *. 100.))}
          %{cell 3 ~label:"latest run" ~color:theme.Styles.secondary latest}
        </div>
      </div>
    |}
  in
  (* One click into a full run: pick a stock and go. *)
  let quick_start =
    let chip symbol =
      let style =
        Styles.s
          ("background:"
           ^ theme.Styles.card_bg
           ^ ";color:"
           ^ theme.Styles.text
           ^ ";border:1px solid "
           ^ theme.Styles.chip_border
           ^ ";border-radius:3px;padding:8px \
              14px;cursor:pointer;font-size:12.5px;font-weight:700;"
           ^ Styles.mono)
      in
      {%html|
        <button
          class="btn"
          %{style}
          on_click=%{fun _ -> quick_start_with symbol}>
          #{Symbol.to_string symbol}
        </button>
      |}
    in
    {%html|
      <div>
        <div %{Styles.s (Styles.label theme ^ "margin-bottom:8px;")}>
          Start with a symbol
        </div>
        <div %{Styles.s "display:flex;gap:8px;flex-wrap:wrap;"}>
          *{List.map Dataset.symbols ~f:chip}
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
          ~back:(Some session_link) ()}
      <div %{Styles.s "display:flex;gap:10px;flex-wrap:wrap;align-items:center;"}>
        %{primary_button ~icon:(Icon.arrow_right ~size:15 ()) ~theme
            ~on_click:(fun _ -> new_sim) "New simulation"}
        %{secondary_button ~theme ~on_click:(fun _ -> to_my_runs)
            "My runs"}
      </div>
      %{stats_band}
      %{quick_start}
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
  ~browse_symbol
  ~set_symbol
  ~selection
  ~select
  ~continue_
  ~profile
  ~on_brand
  ~toggle_theme
  ~back
  =
  let sessions = Dataset.dates_for browse_symbol in
  let session_set = Date.Set.of_list sessions in
  (* The mockup's segmented symbol row: one pill per bundled name. *)
  let symbol_pill symbol =
    let selected = Symbol.equal symbol browse_symbol in
    let style =
      Styles.s
        ("border:1px solid "
         ^ (if selected then theme.Styles.blue else theme.Styles.chip_border)
         ^ ";background:"
         ^ (if selected then theme.Styles.blue else theme.Styles.card_bg)
         ^ ";color:"
         ^ (if selected then "#ffffff" else theme.Styles.secondary)
         ^ ";border-radius:3px;padding:6px \
            12px;font-size:12px;font-weight:700;cursor:pointer;"
         ^ Styles.mono)
    in
    {%html|
      <button class="btn" %{style} on_click=%{fun _ -> set_symbol symbol}>
        #{Symbol.to_string symbol}
      </button>
    |}
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
          match selection with
          | Some (s, d) -> Symbol.equal s browse_symbol && Date.equal d date
          | None -> false
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
            on_click=%{fun _ -> select browse_symbol date}>
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
  let picker_row = Styles.s "display:flex;gap:12px;align-items:center;" in
  let label = Styles.s (Styles.label theme) in
  (* The click-to-reveal preview: sparkline + session stats for the selected
     day, or a prompt when nothing is chosen yet. *)
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
    | None -> empty "Pick a session to preview it"
    | Some (symbol, date) ->
      (match Dataset.load_cached ~symbol ~date with
       | Error (_ : Error.t) -> empty "No data for that session"
       | Ok day ->
         let bars = day.Trading_day.bars in
         let closes =
           List.map bars ~f:(fun bar -> Price.to_float bar.Market_bar.close)
         in
         let open_ = Price.to_float (List.hd_exn bars).Market_bar.open_ in
         let close = List.last_exn closes in
         let volume =
           List.sum (module Int) bars ~f:(fun bar ->
             Size.to_int bar.Market_bar.volume)
         in
         let move_bps = (close -. open_) /. open_ *. 10000. in
         let sparkline =
           let w = 520. in
           let h = 110. in
           let pad = 6. in
           let lo =
             List.min_elt closes ~compare:Float.compare
             |> Option.value ~default:0.
           in
           let hi =
             List.max_elt closes ~compare:Float.compare
             |> Option.value ~default:1.
           in
           let span = Float.max (hi -. lo) 0.01 in
           let count = List.length closes in
           let pt i v =
             sprintf
               "%.1f,%.1f"
               (pad
                +. (Float.of_int i
                    /. Float.of_int (Int.max 1 (count - 1))
                    *. (w -. (2. *. pad))))
               (pad +. ((hi -. v) /. span *. (h -. (2. *. pad))))
           in
           let pts = String.concat ~sep:" " (List.mapi closes ~f:pt) in
           let area =
             sprintf
               "%.1f,%.1f %s %.1f,%.1f"
               pad
               (h -. pad)
               pts
               (w -. pad)
               (h -. pad)
           in
           let svg name attrs children =
             Vdom.Node.create_svg name ~attrs children
           in
           let attr = Vdom.Attr.create in
           svg
             "svg"
             [ attr "viewBox" (sprintf "0 0 %.0f %.0f" w h)
             ; Styles.s "width:100%;display:block;margin:10px 0 14px;"
             ]
             [ svg
                 "polygon"
                 [ attr "points" area
                 ; attr "fill" theme.Styles.blue
                 ; attr "fill-opacity" "0.08"
                 ]
                 []
             ; svg
                 "polyline"
                 [ attr "points" pts
                 ; attr "fill" "none"
                 ; attr "stroke" theme.Styles.blue
                 ; attr "stroke-width" "1.8"
                 ; attr "stroke-linejoin" "round"
                 ]
                 []
             ]
         in
         let stat ~label:text value =
           let tile =
             Styles.s
               ("display:flex;flex-direction:column;gap:3px;border-top:1px \
                 solid "
                ^ theme.Styles.hairline
                ^ ";padding:10px 2px 2px;")
           in
           let value_style =
             Styles.s
               ("font-size:16px;font-weight:700;color:"
                ^ theme.Styles.text
                ^ ";"
                ^ Styles.mono)
           in
           {%html|
             <div %{tile}>
               <span %{Styles.s (Styles.label theme)}>#{text}</span>
               <span %{value_style}>#{value}</span>
             </div>
           |}
         in
         let title_row =
           Styles.s
             ("display:flex;justify-content:space-between;align-items:baseline;gap:14px;flex-wrap:wrap;color:"
              ^ theme.Styles.text
              ^ ";font-size:16px;font-weight:700;")
         in
         let title_name = Styles.s "white-space:nowrap;" in
         let tiles =
           Styles.s
             "display:grid;grid-template-columns:repeat(auto-fit,minmax(110px,1fr));gap:8px;"
         in
         {%html|
           <div %{Styles.card theme "padding:20px;"}>
             <div %{title_row}>
               <span %{title_name}>
                 #{Symbol.to_string symbol} · #{Date.to_string date}
               </span>
               <span %{hint}>1-minute closes · 09:30–15:59 · historical replay, not a forecast</span>
             </div>
             %{sparkline}
             <div %{tiles}>
               %{stat ~label:"open" (sprintf "$%.2f" open_)}
               %{stat ~label:"close" (sprintf "$%.2f" close)}
               %{stat ~label:"day move" (sprintf "%+.0f bps" move_bps)}
               %{stat ~label:"volume"
                   (Float.to_string_hum ~delimiter:',' ~decimals:0
                      (Float.of_int volume))}
             </div>
           </div>
         |})
  in
  let selection_hint =
    match selection with
    | Some ((_ : Symbol.t), date) ->
      sprintf
        "%s selected — your alpha names the symbols"
        (Date.to_string date)
    | None -> "select a session to continue"
  in
  {%html|
    <div class="page fade" %{Styles.s narrow_page}>
      %{wizard_header ~step:0 ?profile ~on_brand ~theme ~is_dark ~toggle_theme
          ~title:"Choose a market day"
          ~subtitle:"pick a symbol, then a session from its calendar — real \
                     historical data, replayed"
          ~back:None ()}
      <div %{Styles.s two_col}>
        <div %{Styles.card theme "padding:20px;"}>
          <div %{picker_row}>
            <span %{label}>Symbol</span>
            <div %{Styles.s "display:flex;gap:4px;flex-wrap:wrap;"}>
              *{List.map Dataset.symbols ~f:symbol_pill}
            </div>
            <span %{hint}>#{sprintf "%d sessions available" (List.length sessions)}</span>
          </div>
          *{List.map months ~f:month_grid}
        </div>
        %{day_preview}
      </div>
      %{nav_footer ~theme
          ~back:("Dashboard", back)
          ~status:selection_hint
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

let sample_alphas ~symbol ~date =
  let s = Symbol.to_string symbol in
  let csv rows =
    String.concat
      ~sep:"\n"
      (List.map rows ~f:(fun (a, side, q, d) ->
         sprintf "%s,%s,%s,%d,%s" a s side q d))
    ^ "\n"
  in
  [ ( "A typical day"
    , "Buy in the morning, sell around noon, buy again after lunch. Start \
       here."
    , csv
        [ "10:00:00", "BUY", 5000, "11:00:00"
        ; "11:30:00", "SELL", 3000, "13:00:00"
        ; "14:00:00", "BUY", 2000, "14:30:00"
        ] )
  ; ( "A morning of buying"
    , "8,000 shares to buy as three overlapping orders, all done by 12:30."
    , csv
        [ "09:45:00", "BUY", 3000, "11:00:00"
        ; "10:15:00", "BUY", 3000, "12:00:00"
        ; "11:00:00", "BUY", 2000, "12:30:00"
        ] )
  ; ( "Buy now, sell later"
    , "Buy 6,000 shares in the morning, then sell them all back in the \
       afternoon."
    , csv
        [ "10:00:00", "BUY", 6000, "11:30:00"
        ; "13:00:00", "SELL", 6000, "15:30:00"
        ] )
  ; ( "Selling into the close"
    , "8,000 shares to sell as three orders, the last ending at 15:55."
    , csv
        [ "13:00:00", "SELL", 2500, "14:30:00"
        ; "13:45:00", "SELL", 2500, "15:00:00"
        ; "14:30:00", "SELL", 3000, "15:55:00"
        ] )
  ; ( "Busy day, six orders"
    , "Six orders alternating buy, sell, buy, sell — 09:40 through 15:45."
    , csv
        [ "09:40:00", "BUY", 1500, "10:30:00"
        ; "10:20:00", "SELL", 1000, "11:15:00"
        ; "11:00:00", "BUY", 2000, "12:30:00"
        ; "12:15:00", "SELL", 1500, "13:45:00"
        ; "13:30:00", "BUY", 1000, "14:30:00"
        ; "14:45:00", "SELL", 2000, "15:45:00"
        ] )
  ; ( "Three at once"
    , "Two buys and a sell all live together from 11:15 to 12:00."
    , csv
        [ "10:30:00", "BUY", 3000, "12:30:00"
        ; "11:00:00", "SELL", 2000, "12:00:00"
        ; "11:15:00", "BUY", 2500, "13:00:00"
        ] )
  ]
  @
  (* Only offered when the day actually has other names to trade: a sample
     that names a symbol with no session for this date would fail on the
     first run, which is a poor introduction. *)
  match others_trading ~symbol ~date with
  | [] -> []
  | others ->
    let names = symbol :: List.take others 2 in
    let rows =
      List.mapi names ~f:(fun index name ->
        let arrival, side, quantity, deadline =
          match index with
          | 0 -> "10:00:00", "BUY", 5000, "11:30:00"
          | 1 -> "10:15:00", "SELL", 4000, "12:00:00"
          | (_ : int) -> "10:45:00", "BUY", 3000, "12:30:00"
        in
        sprintf
          "%s,%s,%s,%d,%s"
          arrival
          (Symbol.to_string name)
          side
          quantity
          deadline)
    in
    [ ( sprintf "%d names at once" (List.length names)
      , sprintf
          "One basket across %s. Each name gets its own book and its own \
           benchmarks; the chart has a tab per symbol."
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
  ~symbol
  ~date
  ~alpha_text
  ~set_alpha_text
  ~continue_
  ~profile
  ~on_brand
  ~toggle_theme
  ~back
  =
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
    let note_box =
      Styles.s
        ("border:1px solid "
         ^ theme.Styles.chip_border
         ^ ";border-radius:3px;padding:10px 12px;margin-top:12px;color:"
         ^ theme.Styles.secondary
         ^ ";font-size:11.5px;line-height:1.6;")
    in
    let note_term =
      Styles.s
        ("color:" ^ theme.Styles.brown ^ ";font-weight:700;" ^ Styles.mono)
    in
    {%html|
      <div %{Styles.card theme "padding:20px;"}>
        %{head}
        *{body}
        <div %{explain}>
          Each instruction is a parent order: arrive at a time, finish by a
          deadline. Malformed rows are flagged with their line number.
        </div>
        <div %{note_box}>
          <span %{note_term}>note</span>
          — an alpha names its own symbols per line, so one file may trade
          any name this date has a session for. Each symbol gets its own
          book and benchmarks; the chart grows a tab per name.
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
            <span %{samples_note}>a run trades what the file names</span>
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
  ~symbol
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
        [ {%html|
            <div %{line}>
              #{sprintf "%s · %d instructions · %s shares" names
                  (List.length instructions)
                  (Int.to_string_hum ~delimiter:','  shares)}
            </div>
          |}
        ; {%html|
            <div %{ready}>
              ✓ ready — validated against session #{Date.to_string date}
            </div>
          |}
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
        </div>
        <div %{note}>
          TWAP and VWAP follow their own schedules and ignore both dials.
        </div>
      </div>
    |}
  in
  let friction_params =
    {%html|
      <div>
        <div %{Styles.s (Styles.label theme ^ "margin-bottom:8px;")}>
          Market friction · house physics
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
  let explain =
    Styles.s
      ("color:"
       ^ theme.Styles.faint
       ^ ";font-size:11px;line-height:1.65;border-top:1px solid "
       ^ theme.Styles.hairline
       ^ ";padding-top:12px;margin-top:18px;max-width:100ch;")
  in
  let params_card =
    {%html|
      <div %{Styles.card theme "padding:20px;"}>
        <div
          %{Styles.s
              "display:grid;grid-template-columns:repeat(auto-fit,minmax(210px,1fr));gap:24px;align-items:start;"}>
          %{preset_rows ~theme ~param_text ~set_param_text}
          %{algo_params}
          %{friction_params}
          %{engine_choice}
        </div>
        <div %{explain}>
          Market friction describes the market you trade against, not your
          strategy: half spread is the toll for demanding an immediate
          fill, the participation cap is the most of one minute's volume
          any single order may take, and the impact coefficient scales how
          far your own trading pushes the price. The bar model prices
          fills by formula from each minute's bar; the synthetic exchange
          runs a real limit order book — background traders post around the
          historical price and your orders match by price-time priority, so
          impact and queueing emerge from the matching. It is slower,
          seeded, and reproducible.
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
                       (alpha_symbols_line ~fallback:symbol alpha_text)
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
  ~to_dashboard
  ~toggle_theme
  ~session
  ~board
  ~submit_status
  ~submit
  ~refresh_board
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
    with_symbol "56px 112px 104px 116px 104px 104px 128px 1fr"
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
    with_symbol "56px 112px 64px 122px 118px 118px 130px 130px 1fr"
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
  let leaderboard_card =
    let title_style =
      Styles.s
        ("color:" ^ theme.Styles.text ^ ";font-size:14px;font-weight:600;")
    in
    let dim =
      Styles.s ("color:" ^ theme.Styles.secondary ^ ";font-size:12.5px;")
    in
    let faint_style =
      Styles.s ("color:" ^ theme.Styles.faint ^ ";font-size:12.5px;")
    in
    (* Who the submission would be attributed to. Guests see the button but
       are routed through sign-in, keeping this finished run in state. *)
    let name_input =
      match (session : Session.t option) with
      | Some { Session.username; token = (_ : string) } ->
        let chip =
          Styles.s
            ("display:inline-flex;align-items:center;gap:6px;background:"
             ^ theme.Styles.blue_soft
             ^ ";color:"
             ^ theme.Styles.blue
             ^ ";border-radius:999px;padding:4px \
                11px;font-size:12px;font-weight:700;")
        in
        {%html|<span %{chip}>#{username}</span>|}
      | None ->
        {%html|<span %{faint_style}>publishing as a guest is not possible</span>|}
    in
    let submit_button =
      let label =
        match (session : Session.t option) with
        | Some (_ : Session.t) -> "Submit to leaderboard"
        | None -> "Sign in to submit"
      in
      primary_button
        ~icon:(Icon.arrow_right ~size:14 ())
        ~theme
        ~on_click:(fun _ -> submit)
        label
    in
    let refresh_button =
      let style =
        Styles.s
          ("background:"
           ^ theme.Styles.chip_bg
           ^ ";color:"
           ^ theme.Styles.secondary
           ^ ";border:1px solid "
           ^ theme.Styles.chip_border
           ^ ";border-radius:5px;padding:7px \
              12px;cursor:pointer;font-size:12.5px;font-weight:600;")
      in
      {%html|
        <button class="btn" %{style} on_click=%{fun _ -> refresh_board}>
          Refresh
        </button>
      |}
    in
    let status =
      match submit_status with
      | None -> []
      | Some text -> [ {%html|<span %{faint_style}>#{text}</span>|} ]
    in
    let columns = "44px 1fr 140px 140px 90px 150px" in
    let head_row =
      Styles.s
        ("display:grid;grid-template-columns:"
         ^ columns
         ^ ";column-gap:10px;white-space:nowrap;padding:10px 16px 6px \
            16px;border-bottom:1px solid "
         ^ theme.Styles.hairline
         ^ ";"
         ^ Styles.table_label theme)
    in
    let board_rows =
      match board with
      | None ->
        [ {%html|
            <div %{Styles.s "padding:10px 16px;"}>
              <span %{faint_style}>
                Submit this run, or refresh to see standing entries.
              </span>
            </div>
          |}
        ]
      | Some [] ->
        [ {%html|
            <div %{Styles.s "padding:10px 16px;"}>
              <span %{faint_style}>
                No submissions for this day and alpha yet — be first.
              </span>
            </div>
          |}
        ]
      | Some rows ->
        List.mapi rows ~f:(fun index (row : Leaderboard_row.t) ->
          let style =
            Styles.s
              ("display:grid;grid-template-columns:"
               ^ columns
               ^ ";column-gap:10px;white-space:nowrap;padding:7px \
                  16px;font-size:12.5px;color:"
               ^ theme.Styles.text
               ^ ";border-bottom:1px solid "
               ^ theme.Styles.hairline
               ^ ";"
               ^ Styles.mono)
          in
          let capture =
            match row.summary.alpha_capture with
            | None -> "n/a"
            | Some capture -> sprintf "%.1f%%" (capture *. 100.)
          in
          {%html|
            <div %{style}>
              <span %{dim}>#{Int.to_string (index + 1)}</span>
              <span>#{row.player}</span>
              <span>%{pnl_cell_int63 ~theme row.summary.value_add_cents}</span>
              <span>%{pnl_cell_int63 ~theme row.summary.net_cents}</span>
              <span %{dim}>#{capture}</span>
              <span %{dim}>#{String.prefix row.submitted_at 16}</span>
            </div>
          |})
    in
    {%html|
      <div %{Styles.card theme "padding-bottom:4px;"}>
        <div
          %{Styles.s
              "display:flex;gap:12px;align-items:center;padding:14px 16px 8px 16px;"}>
          <span %{title_style}>Leaderboard</span>
          <span %{faint_style}>
            · this day · this alpha · this engine · scored by the server
            under house physics
          </span>
          <span
            %{Styles.s
                "margin-left:auto;display:flex;gap:8px;align-items:center;"}>
            *{status}
            %{name_input}
            %{submit_button}
            %{refresh_button}
          </span>
        </div>
        <div %{head_row}>
          <span>rank</span>
          <span>player</span>
          <span>vs immediate</span>
          <span>net P&L</span>
          <span>capture</span>
          <span>submitted</span>
        </div>
        *{board_rows}
      </div>
    |}
  in
  (* The mockup's on-page glossary: every metric's plain-words line in a
     three-column grid; the modal keeps the long-form detail. *)
  let glossary_section =
    let entry (term, plain, (_ : string)) =
      let cell =
        Styles.s
          ("display:flex;flex-direction:column;gap:3px;border-top:1px solid "
           ^ theme.Styles.hairline
           ^ ";padding:10px 0 4px;")
      in
      let term_style =
        Styles.s
          ("color:"
           ^ theme.Styles.text
           ^ ";font-size:12px;font-weight:700;"
           ^ Styles.mono)
      in
      let plain_style =
        Styles.s
          ("color:"
           ^ theme.Styles.secondary
           ^ ";font-size:11.5px;line-height:1.55;")
      in
      {%html|
        <div %{cell}>
          <span %{term_style}>#{term}</span>
          <span %{plain_style}>#{plain}</span>
        </div>
      |}
    in
    {%html|
      <div %{Styles.card theme "padding:16px 18px 14px;"}>
        <div %{Styles.s (Styles.kicker theme ^ "margin-bottom:10px;")}>
          #{sprintf "metrics glossary · %d entries"
              (List.length glossary_entries)}
        </div>
        <div
          %{Styles.s
              "display:grid;grid-template-columns:repeat(auto-fit,minmax(240px,1fr));column-gap:24px;"}>
          *{List.map glossary_entries ~f:entry}
        </div>
      </div>
    |}
  in
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
      %{leaderboard_card}
      %{glossary_section}
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

(* "Five ways to work an order": the algorithm reference data rendered as the
   mockup's comparison table. *)
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
  let aside =
    Styles.s
      ("color:" ^ theme.Styles.faint ^ ";font-size:11.5px;" ^ Styles.mono)
  in
  {%html|
    <div>
      <div %{title_row}>
        <span %{title_style}>Five ways to work an order</span>
        <span %{aside}>same instructions · same day · different execution</span>
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

(* Shared field styling for the sign-in card. *)
let landing_field
  ~theme
  ~kind
  ~label:label_text
  ~value
  ~placeholder
  ~on_input
  =
  let input_style =
    Styles.s
      ("background:"
       ^ theme.Styles.page_bg
       ^ ";color:"
       ^ theme.Styles.text
       ^ ";border:1px solid "
       ^ theme.Styles.chip_border
       ^ ";border-radius:3px;padding:9px 11px;font-size:13.5px;width:100%;"
       ^ Styles.mono)
  in
  {%html|
    <label %{Styles.s "display:flex;flex-direction:column;gap:5px;"}>
      <span %{Styles.s (Styles.label theme)}>#{label_text}</span>
      <input
        type=%{kind}
        %{input_style}
        placeholder=%{placeholder}
        %{Vdom.Attr.string_property "value" value}
        on_input=%{fun (_ : _) text -> on_input text} />
    </label>
  |}
;;

(* The mockup's "Start a session" card: handle + passcode, both auth actions
   side by side, and the guest door underneath. [submit_auth] takes the mode,
   so "Sign in" and "Create account" are one form with two exits. *)
let session_card
  ~theme
  ~username
  ~set_username
  ~passcode
  ~set_passcode
  ~status
  ~submit_auth
  ~enter_as_guest
  =
  let card =
    Styles.card theme "padding:20px 22px;width:100%;max-width:400px;"
  in
  let title_style =
    Styles.s
      ("color:" ^ theme.Styles.text ^ ";font-size:15px;font-weight:700;")
  in
  let note_style =
    Styles.s
      ("color:" ^ theme.Styles.faint ^ ";font-size:11.5px;line-height:1.55;")
  in
  let status_node =
    match status with
    | None -> []
    | Some text ->
      let style =
        Styles.s
          ("color:" ^ theme.Styles.red ^ ";font-size:12px;line-height:1.5;")
      in
      [ {%html|<div %{style}>#{text}</div>|} ]
  in
  let guest_link =
    Styles.s
      ("background:none;border:none;padding:2px 0;color:"
       ^ theme.Styles.blue
       ^ ";font-size:12.5px;font-weight:600;cursor:pointer;text-align:left;"
      )
  in
  let stack = Styles.s "display:flex;flex-direction:column;gap:12px;" in
  {%html|
    <div %{card} id="start">
      <div %{stack}>
        <span %{title_style}>Start a session</span>
        %{landing_field ~theme ~kind:"text" ~label:"handle"
            ~value:username ~placeholder:"handle" ~on_input:set_username}
        %{landing_field ~theme ~kind:"password" ~label:"passcode"
            ~value:passcode ~placeholder:"••••••" ~on_input:set_passcode}
        *{status_node}
        <div %{Styles.s "display:flex;gap:8px;flex-wrap:wrap;"}>
          %{primary_button ~theme
              ~on_click:(fun _ -> submit_auth Auth_mode.Sign_in) "Sign in"}
          %{secondary_button ~theme
              ~on_click:(fun _ -> submit_auth Auth_mode.Create_account)
              "Create account"}
        </div>
        <button
          class="btn"
          %{guest_link}
          on_click=%{fun _ -> enter_as_guest}>
          Continue as guest →
        </button>
        <span %{note_style}>
          An account is only required to publish runs to the leaderboard.
        </span>
      </div>
    </div>
  |}
;;

(* Signed in, the hero card is the way back into the lab instead. *)
let welcome_card ~theme ~username ~enter ~to_my_runs ~sign_out =
  let card =
    Styles.card theme "padding:20px 22px;width:100%;max-width:400px;"
  in
  let who =
    Styles.s
      ("color:" ^ theme.Styles.faint ^ ";font-size:11.5px;" ^ Styles.mono)
  in
  let name_style =
    Styles.s ("color:" ^ theme.Styles.text ^ ";font-weight:700;")
  in
  let link =
    Styles.s
      ("background:none;border:none;padding:2px 0;color:"
       ^ theme.Styles.secondary
       ^ ";font-size:12px;font-weight:600;cursor:pointer;text-align:left;")
  in
  {%html|
    <div %{card} id="start">
      <div %{Styles.s "display:flex;flex-direction:column;gap:12px;"}>
        <span %{who}>signed in as <span %{name_style}>#{username}</span></span>
        %{primary_button ~icon:(Icon.arrow_right ~size:14 ()) ~theme
            ~on_click:(fun _ -> enter) "Start a session"}
        %{secondary_button ~theme ~on_click:(fun _ -> to_my_runs) "My runs"}
        <button class="btn" %{link} on_click=%{fun _ -> sign_out}>
          Sign out
        </button>
      </div>
    </div>
  |}
;;

let landing_view
  ~theme
  ~is_dark
  ~toggle_theme
  ~enter
  ~to_dashboard
  ~session
  ~auth_username
  ~set_auth_username
  ~auth_passcode
  ~set_auth_passcode
  ~auth_status
  ~submit_auth
  ~sign_out
  =
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
  let header_link =
    Styles.s
      ("color:"
       ^ theme.Styles.secondary
       ^ ";font-size:12.5px;font-weight:600;text-decoration:none;padding:6px \
          4px;")
  in
  let header_button =
    Styles.s
      ("color:"
       ^ theme.Styles.text
       ^ ";background:"
       ^ theme.Styles.card_bg
       ^ ";border:1px solid "
       ^ theme.Styles.chip_border
       ^ ";border-radius:3px;padding:6px \
          13px;font-size:12.5px;font-weight:600;text-decoration:none;")
  in
  let right =
    match (session : Session.t option) with
    | None ->
      [ {%html|<a href="#start" class="btn" %{header_link}>Sign in</a>|}
      ; {%html|<a href="#start" class="btn" %{header_button}>Create account</a>|}
      ]
    | Some { Session.username; token = (_ : string) } ->
      let chip =
        Styles.s
          ("color:" ^ theme.Styles.faint ^ ";font-size:11.5px;" ^ Styles.mono)
      in
      let name_style =
        Styles.s ("color:" ^ theme.Styles.text ^ ";font-weight:700;")
      in
      let link =
        Styles.s
          ("background:none;border:none;color:"
           ^ theme.Styles.blue
           ^ ";font-size:12.5px;font-weight:600;cursor:pointer;padding:4px \
              6px;")
      in
      [ {%html|<span %{chip}>signed in as <span %{name_style}>#{username}</span></span>|}
      ; {%html|
          <button class="btn" %{link} on_click=%{fun _ -> to_dashboard}>
            My runs
          </button>
        |}
      ]
  in
  let hero_kicker =
    {%html|<div %{Styles.s (Styles.kicker theme)}>historical execution laboratory</div>|}
  in
  let headline =
    let style =
      Styles.s
        ("color:"
         ^ theme.Styles.text
         ^ ";font-size:clamp(30px,3.8vw,44px);font-weight:700;line-height:1.14;margin:8px \
            0 0;letter-spacing:-0.01em;max-width:15ch;"
         ^ Styles.serif)
    in
    let kept = Styles.s ("color:" ^ theme.Styles.blue ^ ";") in
    {%html|
      <h1 %{style}>
        Your alpha decides <em>what</em> to trade. ExecLab measures what
        execution <span %{kept}>kept</span>.
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
    let strong =
      Styles.s ("color:" ^ theme.Styles.text ^ ";font-weight:600;")
    in
    {%html|
      <div %{style}>
        Upload timestamped parent orders from an alpha model, replay them
        through a real historical session, and watch them execute minute by
        minute. Every basis point you pay to trade is decomposed and
        accounted for — because
        <span %{strong}>the paper profit was never the point; what survives
          execution is</span>.
      </div>
    |}
  in
  let cta =
    let guest_link =
      Styles.s
        ("background:none;border:none;padding:6px 0;color:"
         ^ theme.Styles.blue
         ^ ";font-size:13px;font-weight:600;cursor:pointer;")
    in
    let start_button =
      match (session : Session.t option) with
      | Some (_ : Session.t) ->
        primary_button
          ~icon:(Icon.arrow_right ~size:14 ())
          ~theme
          ~on_click:(fun _ -> enter)
          "Start a session"
      | None ->
        (* Signed out, the button walks you to the session card. *)
        let style =
          Styles.s
            ("display:inline-flex;align-items:center;gap:8px;background:"
             ^ theme.Styles.blue
             ^ ";color:#ffffff;border:1px solid "
             ^ theme.Styles.blue
             ^ ";border-radius:3px;padding:9px \
                18px;font-size:13.5px;font-weight:700;text-decoration:none;"
            )
        in
        {%html|<a href="#start" class="btn" %{style}>Start a session →</a>|}
    in
    {%html|
      <div %{Styles.s "display:flex;gap:14px;align-items:center;flex-wrap:wrap;margin-top:6px;"}>
        %{start_button}
        <button class="btn" %{guest_link} on_click=%{fun _ -> enter}>
          or continue as guest
        </button>
      </div>
    |}
  in
  let hero_card =
    match (session : Session.t option) with
    | None ->
      session_card
        ~theme
        ~username:auth_username
        ~set_username:set_auth_username
        ~passcode:auth_passcode
        ~set_passcode:set_auth_passcode
        ~status:auth_status
        ~submit_auth
        ~enter_as_guest:enter
    | Some { Session.username; token = (_ : string) } ->
      welcome_card ~theme ~username ~enter ~to_my_runs:to_dashboard ~sign_out
  in
  let hero =
    Styles.s
      "display:grid;grid-template-columns:repeat(auto-fit,minmax(340px,1fr));gap:40px;align-items:start;padding-top:10px;"
  in
  {%html|
    <div class="page fade" %{page}>
      <div %{bar}>
        %{wordmark ~theme ()}
        <span %{Styles.s "display:flex;gap:10px;align-items:center;"}>
          *{right}
          %{theme_button ~theme ~is_dark ~toggle_theme}
        </span>
      </div>
      <div %{hero}>
        <div>
          %{hero_kicker}
          %{headline}
          <div %{Styles.s "margin-top:14px;"}>%{subhead}</div>
          %{cta}
        </div>
        <div %{Styles.s "display:flex;justify-content:flex-end;"}>
          %{hero_card}
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

(* ---------- the leaderboard seam ---------- *)

let submit_run_effect = Effect.of_deferred_fun Net.submit_run
let fetch_board_effect = Effect.of_deferred_fun Net.leaderboard
let create_account_effect = Effect.of_deferred_fun Net.create_account
let sign_in_effect = Effect.of_deferred_fun Net.sign_in
let save_run_effect = Effect.of_deferred_fun Net.save_run
let reset_account_effect = Effect.of_deferred_fun Net.reset_account
let my_runs_effect = Effect.of_deferred_fun Net.my_runs

let persist_session =
  Effect.of_sync_fun (fun (session : Session.t option) ->
    Storage.set
      Storage.session_key
      (Sexp.to_string [%sexp (session : Session.t option)]))
;;

let load_session () : Session.t option =
  match Storage.get Storage.session_key with
  | None -> None
  | Some text ->
    (try [%of_sexp: Session.t option] (Sexp.of_string text) with
     | (_ : exn) -> None)
;;

let config_of (replay : Replay.t) ~player =
  let fill = replay.params.Execlab_session.Params.fill_config in
  { Run_config.player
  ; symbols = replay.symbols
  ; date = replay.date
  ; alpha_text = replay.alpha_text
  ; algo_name = replay.algo_name
  ; half_spread_cents = Price.to_int_cents fill.half_spread
  ; max_participation = fill.max_participation
  ; impact_coefficient_cents = Price.to_int_cents fill.impact_coefficient
  ; pov_rate = replay.params.pov_rate
  ; is_urgency = replay.params.is_urgency
  ; engine_name =
      (match replay.params.engine with
       | Execlab_session.Engine_choice.Bar_model -> "bar"
       | Synthetic { seed = (_ : int) } -> "synthetic")
  ; engine_seed =
      (match replay.params.engine with
       | Execlab_session.Engine_choice.Bar_model -> 0
       | Synthetic { seed } -> seed)
  }
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
  let selection, set_selection =
    Bonsai.state (None : (Symbol.t * Date.t) option) graph
  in
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
  let previous_run, set_previous_run =
    Bonsai.state (None : History.Run_record.t option) graph
  in
  let session, set_session =
    Bonsai.state (load_session () : Session.t option) graph
  in
  let auth_username, set_auth_username = Bonsai.state "" graph in
  let auth_passcode, set_auth_passcode = Bonsai.state "" graph in
  let auth_status, set_auth_status =
    Bonsai.state (None : string option) graph
  in
  let my_runs, set_my_runs =
    Bonsai.state (None : Saved_run.t list option) graph
  in
  let confirm_reset, set_confirm_reset = Bonsai.state false graph in
  let board, set_board =
    Bonsai.state (None : Leaderboard_row.t list option) graph
  in
  let submit_status, set_submit_status =
    Bonsai.state (None : string option) graph
  in
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
  let is_dark, set_is_dark = Bonsai.state false graph in
  (* The symbol whose calendar the choose-day screen is browsing; distinct
     from [selection], which is only set once a session is clicked. *)
  let cal_symbol, set_cal_symbol =
    Bonsai.state (None : Symbol.t option) graph
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
    and set_board
    and session
    and set_previous_run
    and set_my_runs
    and set_submit_status in
    match selection, Replay.parse_params param_text with
    | None, _ -> set_run_error (Some (Error.of_string "choose a day first"))
    | Some (_ : Symbol.t * Date.t), Error error -> set_run_error (Some error)
    (* The picked symbol only decides which calendar you were browsing; which
       names actually trade is the alpha's business. *)
    | Some ((_ : Symbol.t), date), Ok params ->
      let%bind.Effect result =
        Effect.of_sync_fun
          (fun () -> Replay.run ~date ~alpha_text ~algo_name:algo ~params)
          ()
      in
      (match result with
       | Error error -> set_run_error (Some error)
       | Ok r ->
         (* A board belongs to one (day, alpha, engine): carrying the
            previous run's rows or its "submitted" banner onto a new run's
            results would be a lie. *)
         let%bind.Effect () = set_board None in
         let%bind.Effect () = set_submit_status None in
         let%bind.Effect () = set_run_error None in
         let%bind.Effect () = set_replay (Some r) in
         let%bind.Effect () = set_minute (fun (_ : int) -> 0) in
         let%bind.Effect () = set_playing true in
         let%bind.Effect () = set_previous_run (List.hd runs) in
         let%bind.Effect () = set_runs (History.add (run_record r) runs) in
         (* Signed-in users get an execution notebook: every completed run is
            recorded server-side, privately, the moment it finishes. The
            server regrades it under the run's own parameters, so the
            notebook never depends on the browser's arithmetic. *)
         let%bind.Effect () =
           match session with
           | None -> Effect.Ignore
           | Some { Session.token; username } ->
             let%bind.Effect (_ : Save_run.Response.t Or_error.t) =
               save_run_effect
                 { Save_run.Request.token
                 ; config = config_of r ~player:username
                 }
             in
             set_my_runs None
         in
         set_screen Screen.Sim)
  in
  let restart =
    let%arr set_minute and set_playing in
    let%bind.Effect () = set_minute (fun (_ : int) -> 0) in
    set_playing true
  in
  (* Signing in or creating an account: one form, and the clicked button
     picks the endpoint. On success the session is persisted, so a reload
     stays signed in. *)
  let submit_auth =
    let%arr auth_username
    and auth_passcode
    and set_session
    and set_auth_status
    and set_auth_passcode
    and set_my_runs
    and set_screen in
    fun (mode : Auth_mode.t) ->
      let credentials =
        { Credentials.username = String.strip auth_username
        ; passcode = auth_passcode
        }
      in
      let%bind.Effect () = set_auth_status (Some "working…") in
      let%bind.Effect response =
        match (mode : Auth_mode.t) with
        | Sign_in -> sign_in_effect credentials
        | Create_account -> create_account_effect credentials
      in
      match response with
      | Error error -> set_auth_status (Some (Error.to_string_hum error))
      | Ok (session : Session.t) ->
        let%bind.Effect () = set_session (Some session) in
        let%bind.Effect () = persist_session (Some session) in
        let%bind.Effect () = set_auth_status None in
        let%bind.Effect () = set_auth_passcode "" in
        (* The dashboard you land on should already be your account's, not
           this browser's leftovers. *)
        let%bind.Effect response =
          my_runs_effect { My_runs.Request.token = session.token }
        in
        let%bind.Effect () =
          set_my_runs
            (match response with
             | Ok resp -> Some resp.My_runs.Response.runs
             | Error (_ : Error.t) -> Some [])
        in
        set_screen Screen.Dashboard
  in
  (* Reopening a notebook entry: the config is complete and the simulator is
     deterministic, so replaying it locally reproduces the exact run rather
     than storing a results blob. *)
  let open_run =
    let%arr set_replay
    and set_run_error
    and set_board
    and set_submit_status
    and set_minute
    and set_screen in
    fun (saved : Saved_run.t) ->
      let config = saved.Saved_run.config in
      let params =
        { Execlab_session.Params.fill_config =
            { half_spread = Price.of_int_cents config.half_spread_cents
            ; max_participation = config.max_participation
            ; impact_coefficient =
                Price.of_int_cents config.impact_coefficient_cents
            }
        ; pov_rate = config.pov_rate
        ; is_urgency = config.is_urgency
        ; engine =
            (match config.engine_name with
             | "synthetic" ->
               Execlab_session.Engine_choice.Synthetic
                 { seed = config.engine_seed }
             | (_ : string) -> Execlab_session.Engine_choice.Bar_model)
        }
      in
      let%bind.Effect result =
        Effect.of_sync_fun
          (fun () ->
            Replay.run
              ~date:config.date
              ~alpha_text:config.alpha_text
              ~algo_name:config.algo_name
              ~params)
          ()
      in
      match result with
      | Error error -> set_run_error (Some error)
      | Ok r ->
        let%bind.Effect () = set_board None in
        let%bind.Effect () = set_submit_status None in
        let%bind.Effect () = set_replay (Some r) in
        let%bind.Effect () =
          set_minute (fun (_ : int) -> Replay.last_minute r)
        in
        set_screen Screen.Results
  in
  (* "Reset all my data": the server notebook and the browser-local run
     history both go. Two clicks — the first only arms the button. *)
  let reset_account =
    let%arr session
    and confirm_reset
    and set_confirm_reset
    and set_my_runs
    and set_runs
    and set_previous_run
    and set_submit_status in
    match session with
    | None -> Effect.Ignore
    | Some { Session.token; username = (_ : string) } ->
      if not confirm_reset
      then set_confirm_reset true
      else (
        let%bind.Effect (_ : Reset_account.Response.t Or_error.t) =
          reset_account_effect { Reset_account.Request.token }
        in
        let%bind.Effect () = set_confirm_reset false in
        let%bind.Effect () = set_my_runs (Some []) in
        let%bind.Effect () = set_previous_run None in
        let%bind.Effect () = set_submit_status None in
        let%bind.Effect () =
          Effect.of_sync_fun (fun () -> History.save []) ()
        in
        set_runs [])
  in
  let sign_out =
    let%arr set_session and set_my_runs and set_screen in
    let%bind.Effect () = set_session None in
    let%bind.Effect () = persist_session None in
    let%bind.Effect () = set_my_runs None in
    set_screen Screen.Landing
  in
  let refresh_my_runs =
    let%arr session and set_my_runs in
    match session with
    | None -> set_my_runs (Some [])
    | Some { Session.token; username = (_ : string) } ->
      let%bind.Effect response = my_runs_effect { My_runs.Request.token } in
      (match response with
       | Ok resp -> set_my_runs (Some resp.My_runs.Response.runs)
       | Error (_ : Error.t) -> set_my_runs (Some []))
  in
  let submit =
    let%arr replay
    and session
    and set_board
    and set_submit_status
    and set_screen in
    match replay, session with
    | None, (_ : Session.t option) -> Effect.Ignore
    (* A guest keeps the finished run in state: we only move them to the
       landing form, and they come back to the same results screen. *)
    | Some (_ : Replay.t), None ->
      let%bind.Effect () =
        set_submit_status
          (Some "sign in or create an account to publish this run")
      in
      set_screen Screen.Landing
    | Some r, Some { Session.token; username } ->
      let%bind.Effect () = set_submit_status (Some "submitting…") in
      let%bind.Effect response =
        submit_run_effect
          { Submit_run.Request.token; config = config_of r ~player:username }
      in
      (match response with
       | Ok resp ->
         let%bind.Effect () =
           set_board (Some resp.Submit_run.Response.leaderboard)
         in
         set_submit_status (Some "published — verified by the server")
       | Error error ->
         set_submit_status
           (Some ("submit failed: " ^ Error.to_string_hum error)))
  in
  let refresh_board =
    let%arr replay and set_board and set_submit_status in
    match replay with
    | None -> Effect.Ignore
    | Some r ->
      let%bind.Effect response =
        fetch_board_effect
          { Leaderboard.Request.symbols = r.symbols
          ; date = r.date
          ; alpha_hash = alpha_hash r.alpha_text
          ; engine_name =
              (match r.params.Execlab_session.Params.engine with
               | Execlab_session.Engine_choice.Bar_model -> "bar"
               | Synthetic { seed = (_ : int) } -> "synthetic")
          }
      in
      (match response with
       | Ok resp -> set_board (Some resp.Leaderboard.Response.rows)
       | Error error ->
         set_submit_status
           (Some ("refresh failed: " ^ Error.to_string_hum error)))
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
  and previous_run
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
  and cal_symbol
  and set_cal_symbol
  and set_minute
  and set_screen
  and start
  and restart
  and session
  and auth_username
  and set_auth_username
  and auth_passcode
  and set_auth_passcode
  and auth_status
  and my_runs
  and submit_auth
  and sign_out
  and reset_account
  and confirm_reset
  and open_run
  and refresh_my_runs
  and board
  and submit_status
  and submit
  and refresh_board in
  let theme = if is_dark then Styles.dark else Styles.paper in
  let toggle_theme =
    let next = if is_dark then Styles.paper else Styles.dark in
    let%bind.Effect () = set_is_dark (not is_dark) in
    set_page_background next.Styles.page_bg
  in
  let goto s = set_screen s in
  let select symbol date = set_selection (Some (symbol, date)) in
  let on_brand = goto Screen.Landing in
  let open_dashboard =
    Effect.Many [ goto Screen.Dashboard; refresh_my_runs ]
  in
  let open_my_runs = Effect.Many [ goto Screen.My_runs; refresh_my_runs ] in
  let profile =
    Some
      (profile_button
         ~theme
         ~session
         ~on_click:
           (match session with
            | Some (_ : Session.t) -> open_dashboard
            | None -> goto Screen.Landing))
  in
  (* Signed in, "your runs" means your account's notebook on the server — the
     same list from any browser. The localStorage history only speaks for
     guests. Int63 cents truncate to int for display; the browser-local path
     has the identical exposure. *)
  let display_runs =
    match session, my_runs with
    | Some (_ : Session.t), Some saved ->
      List.map saved ~f:(fun (run : Saved_run.t) ->
        { History.Run_record.symbols = run.config.symbols
        ; date = run.config.date
        ; algo_name = run.config.algo_name
        ; alpha_capture = run.summary.alpha_capture
        ; value_add_cents = Int63.to_int_trunc run.summary.value_add_cents
        ; net_cents = Int63.to_int_trunc run.summary.net_cents
        ; shortfall_bps = 0.
        ; completion = 0.
        })
    | (Some (_ : Session.t) | None), (Some (_ : Saved_run.t list) | None) ->
      runs
  in
  let dashboard () =
    dashboard_view
      ~theme
      ~is_dark
      ~runs:display_runs
      ~new_sim:(goto Screen.Choose_day)
      ~session
      ~sign_out
      ~to_sign_in:(goto Screen.Landing)
      ~to_my_runs:open_my_runs
      ~quick_start_with:(fun symbol ->
        let%bind.Effect () = set_cal_symbol (Some symbol) in
        set_screen Screen.Choose_day)
      ~profile
      ~on_brand
      ~toggle_theme
  in
  let choose_day () =
    let browse_symbol =
      match cal_symbol, selection with
      | Some symbol, _ -> symbol
      | None, Some (symbol, (_ : Date.t)) -> symbol
      | None, None -> List.hd_exn Dataset.symbols
    in
    choose_day_view
      ~theme
      ~is_dark
      ~browse_symbol
      ~set_symbol:(fun symbol -> set_cal_symbol (Some symbol))
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
        ~to_dashboard:open_my_runs
        ~session
        ~auth_username
        ~set_auth_username
        ~auth_passcode
        ~set_auth_passcode
        ~auth_status
        ~submit_auth
        ~sign_out
    | My_runs, _, _ ->
      my_runs_view
        ~theme
        ~is_dark
        ~toggle_theme
        ~session
        ~profile
        ~on_brand
        ~my_runs
        ~open_run
        ~refresh:refresh_my_runs
        ~reset_account
        ~confirm_reset
        ~new_sim:(goto Screen.Choose_day)
        ~back:open_dashboard
    | Dashboard, _, _ -> dashboard ()
    | Choose_day, _, _ -> choose_day ()
    | (Alpha | Setup | Sim | Results), _, None -> choose_day ()
    | Alpha, _, Some (symbol, date) ->
      alpha_view
        ~theme
        ~is_dark
        ~symbol
        ~date
        ~alpha_text
        ~set_alpha_text
        ~continue_:(goto Screen.Setup)
        ~profile
        ~on_brand
        ~toggle_theme
        ~back:(goto Screen.Choose_day)
    | Setup, _, Some (symbol, date)
    | (Sim | Results), None, Some (symbol, date) ->
      setup_view
        ~theme
        ~is_dark
        ~symbol
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
    | Sim, Some r, Some (_ : Symbol.t * Date.t) ->
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
    | Results, Some r, Some (_ : Symbol.t * Date.t) ->
      results_view
        r
        ~theme
        ~is_dark
        ~open_help:(set_show_help true)
        ~previous_run
        ~profile
        ~on_brand
        ~to_sim:(goto Screen.Sim)
        ~new_sim:(goto Screen.Choose_day)
        ~to_dashboard:open_dashboard
        ~toggle_theme
        ~session
        ~board
        ~submit_status
        ~submit
        ~refresh_board
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

let () =
  Async_js.init ();
  Bonsai_web.Start.start app
;;
