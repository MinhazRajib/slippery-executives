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

(* Light/dark switch; lives in each screen's header. *)
let theme_button ~theme ~is_dark ~toggle_theme =
  let style =
    Styles.s
      ("background:"
       ^ theme.Styles.chip_bg
       ^ ";color:"
       ^ theme.Styles.secondary
       ^ ";border:1px solid "
       ^ theme.Styles.chip_border
       ^ ";border-radius:4px;padding:4px \
          10px;cursor:pointer;font-size:12px;font-weight:600;white-space:nowrap;"
      )
  in
  {%html|
    <button %{style} on_click=%{fun _ -> toggle_theme}>
      #{if is_dark then "☀ light" else "☾ dark"}
    </button>
  |}
;;

module Icon = struct
  let make ?(size = 15) path_d =
    let attr = Vdom.Attr.create in
    Vdom.Node.create_svg
      "svg"
      ~attrs:
        [ attr "width" (Int.to_string size)
        ; attr "height" (Int.to_string size)
        ; attr "viewBox" "0 0 24 24"
        ; attr "fill" "none"
        ; attr "stroke" "currentColor"
        ; attr "stroke-width" "2.2"
        ; attr "stroke-linecap" "round"
        ; attr "stroke-linejoin" "round"
        ; Styles.s "flex-shrink:0;vertical-align:-2px;"
        ]
      [ Vdom.Node.create_svg "path" ~attrs:[ attr "d" path_d ] [] ]
  ;;

  let check = make "M20 6L9 17l-5-5"

  let calendar ?size () =
    make
      ?size
      "M8 2v4M16 2v4M3 10h18M5 4h14a2 2 0 0 1 2 2v14a2 2 0 0 1-2 2H5a2 2 0 \
       0 1-2-2V6a2 2 0 0 1 2-2z"
  ;;

  let file ?size () =
    make
      ?size
      "M14 2H6a2 2 0 0 0-2 2v16a2 2 0 0 0 2 2h12a2 2 0 0 0 2-2V8zM14 2v6h6"
  ;;

  let sliders ?size () =
    make ?size "M4 21v-7M4 10V3M12 21v-9M12 8V3M20 21v-5M20 12V3"
  ;;

  let play ?size () = make ?size "M6 3l14 9-14 9V3z"
  let flag ?size () = make ?size "M4 22V4c5-3 9 3 16 0v12c-7 3-11-3-16 0"

  let upload ?size () =
    make
      ?size
      "M21 15v4a2 2 0 0 1-2 2H5a2 2 0 0 1-2-2v-4M17 8l-5-5-5 5M12 3v12"
  ;;

  let arrow_right ?size () = make ?size "M5 12h14M12 5l7 7-7 7"
  let arrow_left ?size () = make ?size "M19 12H5M12 19l-7-7 7-7"
end

let wizard_steps =
  [ "Day", Icon.calendar ~size:13 ()
  ; "Alpha", Icon.file ~size:13 ()
  ; "Setup", Icon.sliders ~size:13 ()
  ; "Simulate", Icon.play ~size:13 ()
  ; "Results", Icon.flag ~size:13 ()
  ]
;;

let step_progress ~theme ~current =
  let station index (name, icon) =
    let state =
      if index < current
      then `Done
      else if index = current
      then `Active
      else `Upcoming
    in
    let bg, color, weight =
      match state with
      | `Done -> theme.Styles.blue_soft, theme.Styles.blue, "600"
      | `Active -> theme.Styles.blue, theme.Styles.page_bg, "700"
      | `Upcoming -> "transparent", theme.Styles.faint, "600"
    in
    let chip =
      Styles.s
        ("display:inline-flex;align-items:center;gap:6px;background:"
         ^ bg
         ^ ";color:"
         ^ color
         ^ ";border-radius:999px;padding:5px \
            12px;font-size:12px;font-weight:"
         ^ weight
         ^ ";white-space:nowrap;")
    in
    let glyph =
      match state with `Done -> Icon.check | `Active | `Upcoming -> icon
    in
    {%html|<span %{chip}>%{glyph} #{name}</span>|}
  in
  let sep =
    Styles.s ("color:" ^ theme.Styles.faint ^ ";font-size:11px;margin:0 2px;")
  in
  let stations =
    List.concat_mapi wizard_steps ~f:(fun index step ->
      let chip = station index step in
      if index = 0
      then [ chip ]
      else [ {%html|<span %{sep}>—</span>|}; chip ])
  in
  {%html|
    <div
      %{Styles.s
          "display:flex;align-items:center;flex-wrap:wrap;gap:4px;margin-top:10px;"}>
      *{stations}
    </div>
  |}
;;

(* ---------- controls ---------- *)

let pill ~theme ~active ~on_click label =
  let bg = if active then theme.Styles.text else "transparent" in
  let color =
    if active then theme.Styles.page_bg else theme.Styles.secondary
  in
  let style =
    Styles.s
      ("background:"
       ^ bg
       ^ ";color:"
       ^ color
       ^ ";border:none;border-radius:4px;padding:5px \
          12px;cursor:pointer;font-size:13px;font-weight:600;")
  in
  {%html|<button %{style} on_click=%{on_click}>#{label}</button>|}
;;

let controls
  (replay : Replay.t)
  ~theme
  ~minute
  ~playing
  ~speed
  ~zoom
  ~set_playing
  ~set_speed
  ~set_zoom
  ~set_minute
  ~restart
  =
  let last = Replay.last_minute replay in
  let complete = minute >= last in
  let primary =
    Styles.s
      ("background:"
       ^ theme.Styles.brown
       ^ ";color:#ffffff;border:none;border-radius:5px;padding:8px \
          16px;cursor:pointer;font-size:13px;font-weight:700;white-space:nowrap;"
      )
  in
  let group =
    Styles.s
      ("display:flex;gap:2px;background:"
       ^ theme.Styles.chip_bg
       ^ ";border-radius:5px;padding:2px;")
  in
  let slider_style =
    Styles.s
      ("flex:1;accent-color:" ^ theme.Styles.brown ^ ";min-width:160px;")
  in
  let clock_style =
    Styles.s
      ("color:"
       ^ theme.Styles.text
       ^ ";font-size:15px;font-weight:700;"
       ^ Styles.mono)
  in
  let status_text =
    if complete
    then "session complete"
    else if playing
    then "replaying"
    else "paused"
  in
  let status_style =
    Styles.s
      ("color:" ^ theme.Styles.faint ^ ";font-size:13px;white-space:nowrap;")
  in
  let row =
    Styles.s "display:flex;align-items:center;gap:14px;padding:12px 16px;"
  in
  let on_slide (_ : _) value =
    match Int.of_string_opt value with
    | Some v -> set_minute (fun (_ : int) -> Int.max 0 (Int.min last v))
    | None -> Effect.Ignore
  in
  {%html|
    <div %{Styles.card theme ""}>
      <div %{row}>
        <button %{primary} on_click=%{fun _ -> restart}>Replay day</button>
        <div %{group}>
          %{pill
              ~theme
              ~active:(playing && not complete)
              ~on_click:(fun _ -> set_playing (not playing))
              (if playing && not complete then "Pause" else "Play")}
          %{pill ~theme ~active:(speed = 1)
              ~on_click:(fun _ -> set_speed 1) "1x"}
          %{pill ~theme ~active:(speed = 4)
              ~on_click:(fun _ -> set_speed 4) "4x"}
          %{pill ~theme ~active:(speed = 16)
              ~on_click:(fun _ -> set_speed 16) "16x"}
        </div>
        <div %{group}>
          %{pill ~theme ~active:([%equal: int option] zoom None)
              ~on_click:(fun _ -> set_zoom None) "Day"}
          %{pill ~theme ~active:([%equal: int option] zoom (Some 120))
              ~on_click:(fun _ -> set_zoom (Some 120)) "2h"}
          %{pill ~theme ~active:([%equal: int option] zoom (Some 60))
              ~on_click:(fun _ -> set_zoom (Some 60)) "1h"}
          %{pill ~theme ~active:([%equal: int option] zoom (Some 30))
              ~on_click:(fun _ -> set_zoom (Some 30)) "30m"}
          %{pill ~theme ~active:([%equal: int option] zoom (Some 15))
              ~on_click:(fun _ -> set_zoom (Some 15)) "15m"}
        </div>
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
let chart_w = 1140.
let chart_left = 52.
let chart_right = 20.
let chart_plot_w = chart_w -. chart_left -. chart_right

(* The hovered minute, from a mouse event over the chart svg: CSS pixels ->
   viewBox units -> bar index, clamped to the replayed range. *)
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
  ~minute
  ~fills
  ~show_fills
  ~hover
  ~set_hover
  ~view:(z0, z1)
  =
  let bars = replay.bars in
  let n = Array.length bars in
  let w = chart_w in
  let left = chart_left in
  let plot_w = chart_plot_w in
  let top = 10. in
  let price_h = 340. in
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
  (* With a few disjoint orders, full-height tinted windows with arrival
     annotations read well. Once windows overlap or multiply, the tints stack
     into mud and the labels collide — so switch to thin stacked lane bands
     (one per order, tooltip carrying the detail). *)
  let compact_windows =
    let overlaps (a : Replay.parent_replay) (b : Replay.parent_replay) =
      a.arrival_minute < b.deadline_minute
      && b.arrival_minute < a.deadline_minute
    in
    List.length replay.parents > 3
    || List.existsi replay.parents ~f:(fun i a ->
      List.existsi replay.parents ~f:(fun j b -> i < j && overlaps a b))
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
  let window_visible (parent : Replay.parent_replay) =
    parent.arrival_minute <= z1 && parent.deadline_minute >= z0
  in
  let window_edges (parent : Replay.parent_replay) =
    ( x (Int.max z0 parent.arrival_minute)
    , x (Int.min z1 parent.deadline_minute) )
  in
  let windows =
    if compact_windows
    then
      List.filter_mapi replay.parents ~f:(fun index parent ->
        if not (window_visible parent)
        then None
        else (
          let x0, x1 = window_edges parent in
          Some
            (svg
               "rect"
               [ attr "x" (fs x0)
               ; attr "y" (fs (top +. 4. +. (Float.of_int index *. 8.)))
               ; attr "width" (fs (Float.max 3. (x1 -. x0)))
               ; attr "height" (if hovered_in parent then "6" else "4")
               ; attr "rx" "2"
               ; attr "fill" (Styles.order_color theme index)
               ; attr
                   "fill-opacity"
                   (if hovered_in parent then "1" else "0.9")
               ]
               [ window_tooltip index parent ])))
    else
      List.concat_mapi replay.parents ~f:(fun index parent ->
        if not (window_visible parent)
        then []
        else (
          let color = Styles.order_color theme index in
          let x0, x1 = window_edges parent in
          let arrival = Price.to_float parent.arrival_price in
          [ svg
              "rect"
              [ attr "x" (fs x0)
              ; attr "y" (fs top)
              ; attr "width" (fs (Float.max 2. (x1 -. x0)))
              ; attr "height" (fs price_h)
              ; attr "fill" color
              ; attr
                  "fill-opacity"
                  (if hovered_in parent then "0.14" else "0.07")
              ]
              [ window_tooltip index parent ]
          ; svg
              "line"
              [ attr "x1" (fs x0)
              ; attr "x2" (fs x1)
              ; attr "y1" (fs (y arrival))
              ; attr "y2" (fs (y arrival))
              ; attr "stroke" theme.Styles.faint
              ; attr "stroke-width" "1"
              ; attr "stroke-dasharray" "4 3"
              ]
              []
          ; svg
              "text"
              [ attr "x" (fs (x0 +. 4.))
              ; attr "y" (fs (y arrival -. 5.))
              ; attr "fill" theme.Styles.faint
              ; attr "font-size" "11"
              ]
              [ Vdom.Node.text (sprintf "arrival %.2f" arrival) ]
          ]))
  in
  (* the whole-day vwap as a flat dashed reference line *)
  let day_vwap = replay.vwap_by_minute.(n - 1) in
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
  (* optional overlay: each fill as a dot at its executed price *)
  let fill_dots =
    if not show_fills
    then []
    else
      List.filter_map fills ~f:(fun (fill : Fill.t) ->
        let index = Replay.parent_index_of_order replay fill.order_id in
        let m = Replay.minute_of_time replay fill.time in
        if m < z0 || m > z1
        then None
        else
          Some
            (svg
               "circle"
               [ attr "cx" (fs (x m))
               ; attr "cy" (fs (y (Price.to_float fill.price)))
               ; attr "r" "2.2"
               ; attr "fill" (Styles.order_color theme index)
               ; attr "stroke" theme.Styles.card_bg
               ; attr "stroke-width" "0.8"
               ]
               [ tooltip
                   (sprintf
                      "%s %s %d @ %s"
                      (hhmm fill.time)
                      (side_str fill.side)
                      (Size.to_int fill.size)
                      (Price.to_string_dollar fill.price))
               ]))
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
        List.filter_mapi replay.parents ~f:(fun index parent ->
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
    ; Styles.s "width:100%;display:block;cursor:crosshair;"
    ; Vdom.Attr.on_mousemove (fun evt ->
        set_hover (minute_of_mouse ~view:(z0, z1) ~shown:minute evt))
    ; Vdom.Attr.on_mouseleave (fun (_ : _) -> set_hover None)
    ]
    (grid
     @ windows
     @ vwap_line
     @ time_axis
     @ price_line
     @ fill_dots
     @ crosshair)
;;

let legend
  (replay : Replay.t)
  ~theme
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
  (* Per-order legend entries stop earning their space past a few orders; the
     table below carries the mapping instead. *)
  let order_items =
    if List.length replay.parents > 3
    then []
    else
      List.mapi replay.parents ~f:(fun index parent ->
        item
          ~color:(Styles.order_color theme index)
          ~line:false
          (sprintf
             "%s fills (order %d)"
             (side_str parent.instruction.Alpha_instruction.side)
             (index + 1)))
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
    let last = replay.bars.(minute).Market_bar.close in
    let open_pnl = Replay.open_pnl_cents ~fills ~last in
    let stats_style =
      Styles.s "margin-left:auto;display:flex;gap:16px;align-items:center;"
    in
    (* The benefit vs immediate is a whole-day number; revealing it
       mid-replay would spoil the ending. *)
    let benefit =
      if minute >= Replay.last_minute replay
      then
        [ money_stat
            ~theme
            ~label:"Execution benefit"
            replay.results.total_value_add_cents
        ]
      else []
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
          (Symbol.to_string replay.symbol ^ " price (1-min close)")}
      %{item ~color:theme.Styles.orange ~line:true "day vwap"}
      *{order_items}
      %{stats}
      %{toggle}
    </div>
  |}
;;

(* ---------- the orders table ---------- *)

(* One row per parent order: scales to many orders where a card per order
   would not. *)
let orders_table (replay : Replay.t) ~theme ~fills ~minute =
  let columns =
    "92px 96px 116px 78px 96px 148px 62px 112px 112px 82px 1fr"
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
  let day_vwap = replay.vwap_by_minute.(Array.length replay.bars - 1) in
  let now = Replay.time_at replay ~minute in
  let dash =
    let style = Styles.s ("color:" ^ theme.Styles.faint ^ ";") in
    {%html|<span %{style}>-</span>|}
  in
  let row index (parent : Replay.parent_replay) =
    let instruction = parent.instruction in
    let color = Styles.order_color theme index in
    let side = instruction.Alpha_instruction.side in
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
        let text = sprintf "$%.4f" a in
        {%html|<span>#{text}</span>|}
    in
    {%html|
      <div %{style}>
        <span %{order_label}><span %{chip}></span>Order %{index + 1#Int}</span>
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
  {%html|
    <div %{Styles.card theme "padding-bottom:4px;"}>
      <div %{header}><span %{title_style}>Orders</span></div>
      <div %{head_row}>
        <span>order</span>
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
                (Symbol.to_string replay.symbol)
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
                    "complete · %s filled · avg $%.4f"
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
      "max-height:280px;overflow-y:auto;scrollbar-width:thin;display:flex;flex-direction:column-reverse;padding:8px \
       0;"
  in
  let header =
    Styles.s
      ("padding:14px 16px 8px 16px;border-bottom:1px solid "
       ^ theme.Styles.hairline
       ^ ";")
  in
  let count = sprintf " · %d events" (List.length events) in
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

let primary_button ?(enabled = true) ?icon ~theme ~on_click label =
  let style =
    Styles.s
      ("display:inline-flex;align-items:center;gap:8px;background:"
       ^ theme.Styles.blue
       ^ ";color:"
       ^ theme.Styles.page_bg
       ^ ";border:none;border-radius:8px;padding:13px \
          26px;cursor:pointer;font-size:15px;font-weight:700;align-self:flex-start;box-shadow:0 \
          2px 10px "
       ^ theme.Styles.blue
       ^ "44;")
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
       ^ theme.Styles.chip_bg
       ^ ";color:"
       ^ theme.Styles.text
       ^ ";border:1px solid "
       ^ theme.Styles.chip_border
       ^ ";border-radius:8px;padding:12px \
          22px;cursor:pointer;font-size:14px;font-weight:600;")
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
let nav_footer ?back ?next ~theme () =
  let bar =
    Styles.s
      ("position:sticky;bottom:12px;display:flex;justify-content:space-between;align-items:center;gap:12px;background:"
       ^ theme.Styles.card_bg
       ^ ";border:"
       ^ theme.Styles.border
       ^ ";border-radius:10px;padding:12px 16px;"
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
      %{next_node}
    </div>
  |}
;;

let sim_view
  (replay : Replay.t)
  ~theme
  ~is_dark
  ~minute
  ~playing
  ~speed
  ~show_fills
  ~zoom
  ~set_playing
  ~set_speed
  ~set_zoom
  ~set_minute
  ~restart
  ~toggle_fills
  ~toggle_theme
  ~to_results
  ~back
  ~hover
  ~set_hover
  =
  let fills = Replay.fills_upto replay ~minute in
  (* The visible bar window: full session, or a preset span centered on the
     playhead (clamped at the edges), so the zoom follows the replay. *)
  let view =
    let n = Array.length replay.bars in
    match zoom with
    | None -> 0, n - 1
    | Some span ->
      let span = Int.min span (n - 1) in
      let z0 = Int.max 0 (Int.min (minute - (span / 2)) (n - 1 - span)) in
      z0, z0 + span
  in
  let page =
    Styles.s
      "display:flex;flex-direction:column;gap:16px;max-width:1240px;margin:0 \
       auto;padding:28px 20px;"
  in
  let title_style =
    Styles.s
      ("color:"
       ^ theme.Styles.text
       ^ ";font-size:20px;font-weight:700;margin:4px 0;")
  in
  let sub_style =
    Styles.s
      ("color:" ^ theme.Styles.secondary ^ ";font-size:13px;line-height:1.7;")
  in
  let head_row =
    Styles.s
      "display:flex;align-items:baseline;justify-content:space-between;"
  in
  let title =
    sprintf
      "%s · %s · %s"
      (Symbol.to_string replay.symbol)
      (Date.to_string replay.date)
      (String.uppercase replay.algo_name)
  in
  let command =
    sprintf
      "dune exec bin/main.exe -- <your_alpha.csv> %s %s %s"
      (Symbol.to_string replay.symbol)
      (Date.to_string replay.date)
      replay.algo_name
  in
  {%html|
    <div class="page fade" %{page}>
      <div>
        <div %{head_row}>
          <span %{Styles.brand theme}>execlab</span>
          <span %{Styles.s "display:flex;gap:10px;align-items:center;"}>
            %{theme_button ~theme ~is_dark ~toggle_theme}
          </span>
        </div>
        <div %{title_style}>#{title}</div>
        <div %{sub_style}>
          source: <span %{Styles.code_chip theme}>#{command}</span>
        </div>
        %{step_progress ~theme ~current:3}
      </div>
      %{controls replay ~theme ~minute ~playing ~speed ~zoom ~set_playing
          ~set_speed ~set_zoom ~set_minute ~restart}
      <div %{Styles.card theme "padding-bottom:8px;"}>
        %{legend replay ~theme ~minute ~fills ~show_fills ~toggle_fills}
        %{chart replay ~theme ~minute ~fills ~show_fills ~hover
            ~set_hover ~view}
      </div>
      %{orders_table replay ~theme ~fills ~minute}
      %{event_log replay ~theme ~fills ~minute}
      %{nav_footer ~theme
          ~back:("New simulation", back)
          ~next:("Results", to_results, true) ()}
    </div>
  |}
;;

(* Small inline SVG glyphs (Lucide outlines), stroke = currentColor so they
   inherit the surrounding text color in either theme. *)
let algo_pill ~theme ~selected ~on_click label =
  let bg = if selected then theme.Styles.blue else theme.Styles.chip_bg in
  let color =
    if selected then theme.Styles.page_bg else theme.Styles.secondary
  in
  let ring =
    if selected
    then "box-shadow:0 2px 8px " ^ theme.Styles.blue ^ "55;"
    else ""
  in
  let style =
    Styles.s
      ("display:inline-flex;align-items:center;gap:7px;background:"
       ^ bg
       ^ ";color:"
       ^ color
       ^ ";border:none;border-radius:8px;padding:10px \
          20px;cursor:pointer;font-size:14px;font-weight:700;"
       ^ ring)
  in
  let mark = if selected then [ Icon.check ] else [] in
  {%html|
    <button class="btn" %{style} on_click=%{on_click}>
      *{mark}
      #{label}
    </button>
  |}
;;

let instruction_row ~theme (instruction : Alpha_instruction.t) =
  let row =
    Styles.s
      ("display:flex;gap:16px;padding:8px 0;border-bottom:1px solid "
       ^ theme.Styles.hairline
       ^ ";font-size:13px;color:"
       ^ theme.Styles.text
       ^ ";"
       ^ Styles.mono)
  in
  let side_style = Styles.s "font-weight:700;width:44px;" in
  let dim = Styles.s ("color:" ^ theme.Styles.secondary ^ ";") in
  let qty =
    sprintf
      "%s %s"
      (Int.to_string_hum ~delimiter:',' (Size.to_int instruction.quantity))
      (Symbol.to_string instruction.symbol)
  in
  let window =
    sprintf
      "%s → %s"
      (hhmm instruction.arrival_time)
      (hhmm instruction.deadline)
  in
  {%html|
    <div %{row}>
      <span %{side_style}>#{side_str instruction.side}</span>
      <span>#{qty}</span>
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
  ~theme
  ~is_dark
  ~toggle_theme
  ~title
  ~subtitle
  ~back
  ()
  =
  let title_style =
    Styles.s
      ("color:"
       ^ theme.Styles.text
       ^ ";font-size:26px;font-weight:800;margin:4px 0;letter-spacing:0;")
  in
  let sub_style =
    Styles.s ("color:" ^ theme.Styles.secondary ^ ";font-size:14px;")
  in
  let back_style =
    Styles.s
      ("background:none;border:none;color:"
       ^ theme.Styles.blue
       ^ ";cursor:pointer;font-size:13px;font-weight:600;padding:6px 8px;")
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
    <div
      %{Styles.s
          "display:flex;justify-content:space-between;align-items:flex-start;gap:12px;flex-wrap:wrap;"}>
      <div>
        <span %{Styles.brand theme}>execlab</span>
        <div %{title_style}>#{title}</div>
        <div %{sub_style}>#{subtitle}</div>
        *{progress}
      </div>
      <span %{Styles.s "display:flex;gap:10px;align-items:center;"}>
        *{back_button}
        %{theme_button ~theme ~is_dark ~toggle_theme}
      </span>
    </div>
  |}
;;

let narrow_page =
  "display:flex;flex-direction:column;gap:16px;max-width:1240px;margin:32px \
   auto;padding:20px;"
;;

(* Side-by-side halves for the wizard screens; collapses on narrow windows. *)
let two_col =
  "display:grid;grid-template-columns:repeat(auto-fit,minmax(420px,1fr));gap:16px;align-items:start;"
;;

(* ---------- dashboard ---------- *)

(* ---------- landing page ---------- *)

let landing_stats =
  [ "66", "historical sessions — 6 symbols x 11 days, July 2026"
  ; "25,740", "one-minute bars of real prices and volume"
  ; "5", "execution algorithms, from naive to adaptive"
  ; "2", "fill engines: bar model and synthetic order book"
  ]
;;

let landing_sections =
  [ ( "How a run works"
    , "Five steps, no setup. Bring a CSV of trade instructions, pick a day \
       and an algorithm, and watch the fills land against that day's real \
       tape."
    , [ ( "1. Pick a day"
        , "One symbol, one real session from the bundled data: AAPL, GOOG, \
           META, MSFT, NFLX, TSLA." )
      ; ( "2. Load your alpha"
        , "A CSV your model already produced — time, symbol, side, \
           quantity, deadline — or start from a built-in sample." )
      ; ( "3. Choose an algorithm"
        , "TWAP, VWAP, POV, IS, or Immediate, with the market knobs that \
           matter: half spread, participation cap, impact." )
      ; ( "4. Watch it trade"
        , "390 minutes on a scrubber at 1x, 4x or 16x: price chart, \
           per-order fill ticks, running P&L, event log." )
      ; ( "5. Get graded"
        , "Every run is scored against the Immediate baseline on the same \
           day, so you always see what your algorithm was worth." )
      ] )
  ; ( "The five algorithms"
    , "Each one is a different bet about the day. They fail in different \
       ways, and finding out where is the point of the lab."
    , [ ( "TWAP"
        , "Even slices across the clock. Ignores the market entirely — \
           either discipline or negligence, depending on the session." )
      ; ( "VWAP"
        , "Follows the forecast volume curve, heavier at the open and \
           close. Promises a finish time, not a market share." )
      ; ( "POV"
        , "Chases the volume that actually prints, at a fixed share of it. \
           Promises a market share, not a finish time." )
      ; ( "IS"
        , "Implementation shortfall: trades the cost of moving fast against \
           the risk of moving slow, and re-decides every minute." )
      ; ( "Immediate"
        , "Dump the whole order at once. The naive baseline every run is \
           measured against — and it is not always the loser." )
      ] )
  ; ( "What gets measured"
    , "The headline is alpha captured: what you kept divided by what the \
       idea was worth on paper. Everything below it explains where the rest \
       went."
    , [ ( "Implementation shortfall"
        , "Your average fill price against the price at the moment you \
           decided, in dollars and basis points." )
      ; ( "Cost split"
        , "Shortfall broken into timing (the price moved), spread (the toll \
           on demanding a fill), and impact (you moved it)." )
      ; ( "Opportunity cost"
        , "Shares that never traded before the deadline. A correct signal \
           you failed to trade is a pure loss, and it is priced here." )
      ; ( "VWAP slippage"
        , "Your average price against the whole day's average. Did you \
           trade better or worse than everyone else?" )
      ; ( "Value added"
        , "Your net P&L minus the Immediate baseline's, on the identical \
           day — the number the leaderboard ranks." )
      ] )
  ; ( "The market model, stated plainly"
    , "ExecLabs is a simulation calibrated to a real historical session, \
       not a reconstruction of the real order book. The bars are real; the \
       spread, the queue and the counterparties are modeled."
    , [ ( "Bar fill engine"
        , "Marketable orders fill at the bar price plus a half spread and a \
           square-root impact penalty, capped at a share of that minute's \
           volume." )
      ; ( "Synthetic exchange"
        , "A real limit order book with background agents. Impact emerges \
           from price-time priority instead of a formula." )
      ; ( "Deterministic replay"
        , "The same config and seed give identical fills every time. Runs \
           are reproducible artifacts, not anecdotes." )
      ; ( "Calibrated, not reconstructed"
        , "We never saw the true book. What is trustworthy is the \
           comparison: every run faces the same distortions." )
      ; ( "Server-verified leaderboard"
        , "Submit a config and the server re-runs it under identical house \
           physics. Nobody uploads a score, so only execution differs." )
      ] )
  ]
;;

let landing_view ~theme ~is_dark ~toggle_theme ~enter ~to_dashboard =
  let page =
    Styles.s
      "display:flex;flex-direction:column;gap:28px;max-width:1240px;margin:0 \
       auto;padding:36px 20px 60px;"
  in
  let brand_row =
    Styles.s "display:flex;justify-content:space-between;align-items:center;"
  in
  let wordmark =
    Styles.s
      ("color:"
       ^ theme.Styles.text
       ^ ";font-size:17px;font-weight:800;letter-spacing:0.01em;")
  in
  let hero =
    Styles.s
      "display:flex;flex-direction:column;gap:16px;padding:28px 0 8px;"
  in
  let headline =
    Styles.s
      ("color:"
       ^ theme.Styles.text
       ^ ";font-size:clamp(30px,4.6vw,52px);font-weight:800;line-height:1.08;max-width:16ch;margin:0;"
      )
  in
  let subhead =
    Styles.s
      ("color:"
       ^ theme.Styles.secondary
       ^ ";font-size:16.5px;line-height:1.65;max-width:64ch;")
  in
  let accent =
    Styles.s ("color:" ^ theme.Styles.blue ^ ";font-weight:700;")
  in
  let cta_row =
    Styles.s "display:flex;gap:12px;flex-wrap:wrap;margin-top:6px;"
  in
  let stat_grid =
    Styles.s
      "display:grid;grid-template-columns:repeat(auto-fit,minmax(210px,1fr));gap:12px;"
  in
  let stat (value, label) =
    let tile =
      Styles.s
        ("display:flex;flex-direction:column;gap:4px;background:"
         ^ theme.Styles.chip_bg
         ^ ";border:1px solid "
         ^ theme.Styles.chip_border
         ^ ";border-radius:10px;padding:16px 18px;")
    in
    let value_style =
      Styles.s
        ("color:"
         ^ theme.Styles.blue
         ^ ";font-size:27px;font-weight:800;"
         ^ Styles.mono)
    in
    let label_style =
      Styles.s
        ("color:"
         ^ theme.Styles.secondary
         ^ ";font-size:12.5px;line-height:1.5;")
    in
    {%html|
      <div class="raise" %{tile}>
        <span %{value_style}>#{value}</span>
        <span %{label_style}>#{label}</span>
      </div>
    |}
  in
  let section (title, body, items) =
    let title_style =
      Styles.s
        ("color:"
         ^ theme.Styles.text
         ^ ";font-size:21px;font-weight:800;margin-bottom:6px;")
    in
    let body_style =
      Styles.s
        ("color:"
         ^ theme.Styles.secondary
         ^ ";font-size:14px;line-height:1.65;max-width:70ch;margin-bottom:14px;"
        )
    in
    let grid =
      Styles.s
        "display:grid;grid-template-columns:repeat(auto-fit,minmax(230px,1fr));gap:12px;"
    in
    let item (name, blurb) =
      let card =
        Styles.s
          ("display:flex;flex-direction:column;gap:5px;background:"
           ^ theme.Styles.card_bg
           ^ ";border:"
           ^ theme.Styles.border
           ^ ";border-radius:10px;padding:16px 18px;height:100%;")
      in
      let name_style =
        Styles.s
          ("color:"
           ^ theme.Styles.text
           ^ ";font-size:13.5px;font-weight:700;")
      in
      let blurb_style =
        Styles.s
          ("color:"
           ^ theme.Styles.secondary
           ^ ";font-size:12.5px;line-height:1.6;")
      in
      {%html|
        <div class="raise" %{card}>
          <span %{name_style}>#{name}</span>
          <span %{blurb_style}>#{blurb}</span>
        </div>
      |}
    in
    {%html|
      <div>
        <div %{title_style}>#{title}</div>
        <div %{body_style}>#{body}</div>
        <div %{grid}>*{List.map items ~f:item}</div>
      </div>
    |}
  in
  let closing =
    Styles.s
      ("color:"
       ^ theme.Styles.secondary
       ^ ";font-size:15px;line-height:1.6;border-top:1px solid "
       ^ theme.Styles.hairline
       ^ ";padding-top:22px;")
  in
  {%html|
    <div class="page fade" %{page}>
      <div %{brand_row}>
        <span %{wordmark}>ExecLabs</span>
        %{theme_button ~theme ~is_dark ~toggle_theme}
      </div>
      <div %{hero}>
        <h1 %{headline}>Backtest your execution, not just your alpha.</h1>
        <div %{subhead}>
          Your model says buy 50,000 shares by 11:00. The price you actually
          get is not the price on the chart. ExecLabs replays your orders
          minute by minute against a real historical trading session and
          reports <span %{accent}>how much of the paper profit survived the
          cost of trading it</span>.
        </div>
        <div %{cta_row}>
          %{primary_button ~icon:(Icon.arrow_right ~size:15 ()) ~theme
              ~on_click:(fun _ -> enter) "Run a simulation"}
          %{secondary_button ~theme ~on_click:(fun _ -> to_dashboard)
              "Browse past runs"}
        </div>
      </div>
      <div %{stat_grid}>*{List.map landing_stats ~f:stat}</div>
      *{List.map landing_sections ~f:section}
      <div %{closing}>
        Bring the orders your model already generated. Find out what they
        actually cost.
      </div>
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
       ^ ";border-radius:12px;max-width:760px;width:100%;padding:24px;"
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

let dashboard_view ~theme ~is_dark ~runs ~new_sim ~toggle_theme =
  let section_label = Styles.s (Styles.label theme ^ "margin-bottom:8px;") in
  let empty_style =
    Styles.s ("color:" ^ theme.Styles.faint ^ ";font-size:13px;")
  in
  let row_base =
    "display:grid;grid-template-columns:150px 90px 1fr 1fr \
     70px;column-gap:10px;align-items:baseline;"
  in
  let head_row =
    Styles.s
      (row_base
       ^ "padding:8px 0 6px 0;border-bottom:1px solid "
       ^ theme.Styles.hairline
       ^ ";"
       ^ Styles.table_label theme)
  in
  let run_row (run : History.Run_record.t) =
    let style =
      Styles.s
        (row_base
         ^ "padding:7px 0;font-size:13px;color:"
         ^ theme.Styles.text
         ^ ";border-bottom:1px solid "
         ^ theme.Styles.hairline
         ^ ";"
         ^ Styles.mono)
    in
    let dim = Styles.s ("color:" ^ theme.Styles.secondary ^ ";") in
    let capture =
      match run.alpha_capture with
      | None -> "n/a"
      | Some c -> sprintf "%.1f%%" (c *. 100.)
    in
    {%html|
      <div %{style}>
        <span>#{Symbol.to_string run.symbol} · #{Date.to_string run.date}</span>
        <span %{dim}>#{String.uppercase run.algo_name}</span>
        <span>%{money_stat ~theme ~label:"value add" run.value_add_cents}</span>
        <span>%{money_stat ~theme ~label:"net" run.net_cents}</span>
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
  {%html|
    <div class="page fade" %{Styles.s narrow_page}>
      %{wizard_header ~theme ~is_dark ~toggle_theme
          ~title:"Historical execution laboratory"
          ~subtitle:"upload an alpha, pick a day, and see how much survives \
                     execution" ~back:None ()}
      %{primary_button ~icon:(Icon.arrow_right ~size:14 ()) ~theme
          ~on_click:(fun _ -> new_sim) "New simulation"}
      <div %{Styles.s two_col}>
        <div %{Styles.card theme "padding:20px;"}>
          <div %{section_label}>Recent runs</div>
          *{table runs}
        </div>
        <div %{Styles.card theme "padding:20px;"}>
          <div %{section_label}>Best runs — by value added vs immediate</div>
          *{table best}
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
  ~toggle_theme
  ~back
  =
  let sessions = Dataset.dates_for browse_symbol in
  let session_set = Date.Set.of_list sessions in
  let select_style =
    Styles.s
      ("background:"
       ^ theme.Styles.chip_bg
       ^ ";color:"
       ^ theme.Styles.text
       ^ ";border:1px solid "
       ^ theme.Styles.chip_border
       ^ ";border-radius:5px;padding:6px \
          10px;font-size:13px;font-weight:600;cursor:pointer;"
       ^ Styles.mono)
  in
  let symbol_option symbol =
    let name = Symbol.to_string symbol in
    let selected_prop =
      if Symbol.equal symbol browse_symbol
      then Vdom.Attr.bool_property "selected" true
      else Vdom.Attr.empty
    in
    {%html|<option value=%{name} %{selected_prop}>#{name}</option>|}
  in
  let hint =
    Styles.s
      ("color:" ^ theme.Styles.faint ^ ";font-size:12px;" ^ Styles.mono)
  in
  let cell_base =
    "width:56px;height:42px;display:flex;align-items:center;justify-content:center;border-radius:8px;font-size:13.5px;"
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
               ("display:flex;flex-direction:column;gap:2px;background:"
                ^ theme.Styles.chip_bg
                ^ ";border-radius:8px;padding:10px 14px;")
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
             ("display:flex;justify-content:space-between;align-items:baseline;color:"
              ^ theme.Styles.text
              ^ ";font-size:16px;font-weight:700;")
         in
         let tiles =
           Styles.s
             "display:grid;grid-template-columns:repeat(auto-fit,minmax(110px,1fr));gap:8px;"
         in
         {%html|
           <div %{Styles.card theme "padding:20px;"}>
             <div %{title_row}>
               <span>
                 #{Symbol.to_string symbol} · #{Date.to_string date}
               </span>
               <span %{hint}>1-minute closes, 09:30–15:59</span>
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
    | Some (symbol, date) ->
      sprintf "%s · %s" (Symbol.to_string symbol) (Date.to_string date)
    | None -> "select a session to continue"
  in
  {%html|
    <div class="page fade" %{Styles.s narrow_page}>
      %{wizard_header ~step:0 ~theme ~is_dark ~toggle_theme
          ~title:"Choose a market day"
          ~subtitle:"pick a symbol, then a session from its calendar"
          ~back:None ()}
      <div %{Styles.s two_col}>
        <div %{Styles.card theme "padding:20px;"}>
          <div %{picker_row}>
            <span %{label}>Symbol</span>
            <select
              class="btn"
              %{select_style}
              on_change=%{fun (_ : _) value -> set_symbol (Symbol.of_string value)}>
              *{List.map Dataset.symbols ~f:symbol_option}
            </select>
            <span %{hint}>#{sprintf "%d sessions available" (List.length sessions)}</span>
          </div>
          *{List.map months ~f:month_grid}
        </div>
        %{day_preview}
      </div>
      <div %{Styles.s ("display:flex;justify-content:flex-end;" ^ Styles.label theme)}>
        #{selection_hint}
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
let sample_alphas symbol =
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
  ~toggle_theme
  ~back
  =
  let section_label = Styles.s (Styles.label theme ^ "margin-bottom:8px;") in
  let hint =
    Styles.s
      ("color:" ^ theme.Styles.faint ^ ";font-size:12px;" ^ Styles.mono)
  in
  let textarea_style =
    Styles.s
      ("width:100%;box-sizing:border-box;background:"
       ^ theme.Styles.page_bg
       ^ ";color:"
       ^ theme.Styles.text
       ^ ";border:1px solid "
       ^ theme.Styles.chip_border
       ^ ";border-radius:5px;padding:10px;font-size:13px;resize:vertical;"
       ^ Styles.mono)
  in
  let error_style =
    Styles.s
      ("color:" ^ theme.Styles.red ^ ";font-size:12.5px;" ^ Styles.mono)
  in
  let parsed = Replay.parse_alpha alpha_text in
  let preview =
    match parsed with
    | Ok instructions ->
      [ {%html|
          <div %{Styles.card theme "padding:20px;"}>
            <div %{section_label}>Parsed instructions</div>
            *{List.map instructions ~f:(instruction_row ~theme)}
          </div>
        |}
      ]
    | Error error ->
      [ {%html|
          <div %{Styles.card theme "padding:20px;"}>
            <div %{section_label}>Parse errors</div>
            <div %{error_style}>#{Error.to_string_hum error}</div>
          </div>
        |}
      ]
  in
  let sample_card (name, description, csv) =
    let selected = String.equal alpha_text csv in
    let style =
      Styles.s
        ("display:flex;flex-direction:column;gap:3px;text-align:left;background:"
         ^ (if selected then theme.Styles.blue_soft else theme.Styles.chip_bg)
         ^ ";color:"
         ^ theme.Styles.text
         ^ ";border:1px solid "
         ^ (if selected then theme.Styles.blue else theme.Styles.chip_border)
         ^ ";border-radius:8px;padding:10px 12px;cursor:pointer;")
    in
    let name_style =
      Styles.s
        ("font-size:13px;font-weight:700;color:"
         ^ (if selected then theme.Styles.blue else theme.Styles.text)
         ^ ";")
    in
    let desc_style =
      Styles.s ("font-size:11.5px;color:" ^ theme.Styles.secondary ^ ";")
    in
    let mark = if selected then [ Icon.check ] else [] in
    {%html|
      <button class="btn" %{style} on_click=%{fun _ -> set_alpha_text csv}>
        <span %{Styles.s "display:flex;align-items:center;gap:6px;"}>
          *{mark}
          <span %{name_style}>#{name}</span>
        </span>
        <span %{desc_style}>#{description}</span>
      </button>
    |}
  in
  let samples_row =
    Styles.s
      "display:grid;grid-template-columns:repeat(auto-fill,minmax(200px,1fr));gap:8px;margin-bottom:12px;"
  in
  let upload_label =
    Styles.s
      ("display:inline-flex;align-items:center;gap:8px;background:"
       ^ theme.Styles.chip_bg
       ^ ";color:"
       ^ theme.Styles.text
       ^ ";border:1px solid "
       ^ theme.Styles.chip_border
       ^ ";border-radius:5px;padding:6px \
          12px;cursor:pointer;font-size:12.5px;font-weight:600;margin-top:10px;"
      )
  in
  let subtitle =
    sprintf
      "%s · %s — paste, upload, or start from a sample"
      (Symbol.to_string symbol)
      (Date.to_string date)
  in
  {%html|
    <div class="page fade" %{Styles.s narrow_page}>
      %{wizard_header ~step:1 ~theme ~is_dark ~toggle_theme
          ~title:"Alpha instructions" ~subtitle
          ~back:None ()}
      <div %{Styles.s two_col}>
        <div %{Styles.card theme "padding:20px;"}>
          <div %{section_label}>Samples</div>
          <div
            %{Styles.s
                ("color:"
                 ^ theme.Styles.faint
                 ^ ";font-size:11.5px;margin-bottom:8px;")}>
            written for #{Symbol.to_string symbol} — the symbol you picked;
            a run trades one stock per session (multi-stock strategies are
            on the roadmap)
          </div>
          <div %{samples_row}>
            *{List.map (sample_alphas symbol) ~f:sample_card}
          </div>
          <div %{section_label}>Alpha CSV</div>
          <textarea
            rows=%{14}
            %{Vdom.Attr.create "spellcheck" "false"}
            %{Vdom.Attr.string_property "value" alpha_text}
            %{textarea_style}
            on_input=%{fun (_ : _) text -> set_alpha_text text}></textarea>
          <div %{hint}>arrival_time,symbol,side,quantity,deadline</div>
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
        </div>
        <div %{Styles.s "display:flex;flex-direction:column;gap:16px;"}>
          *{preview}
        </div>
      </div>
      %{nav_footer ~theme
          ~back:("Choose day", back)
          ~next:("Continue", continue_, Or_error.is_ok parsed) ()}
    </div>
  |}
;;

(* ---------- algorithm + confirm ---------- *)

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
  ~toggle_theme
  ~back
  =
  let section_label = Styles.s (Styles.label theme ^ "margin-bottom:8px;") in
  let pills = Styles.s "display:flex;gap:8px;" in
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
  let instructions =
    match Replay.parse_alpha alpha_text with
    | Ok instructions -> List.map instructions ~f:(instruction_row ~theme)
    | Error error ->
      [ {%html|<div %{error_style}>#{Error.to_string_hum error}</div>|} ]
  in
  let subtitle =
    sprintf
      "%s · %s · bar-based fill model"
      (Symbol.to_string symbol)
      (Date.to_string date)
  in
  let param_field ~label:text ~value ~set =
    let input_style =
      Styles.s
        ("width:110px;background:"
         ^ theme.Styles.page_bg
         ^ ";color:"
         ^ theme.Styles.text
         ^ ";border:1px solid "
         ^ theme.Styles.chip_border
         ^ ";border-radius:4px;padding:5px 8px;font-size:12.5px;"
         ^ Styles.mono)
    in
    let label_style =
      Styles.s ("color:" ^ theme.Styles.secondary ^ ";font-size:12px;")
    in
    {%html|
      <label %{Styles.s "display:flex;flex-direction:column;gap:4px;"}>
        <span %{label_style}>#{text}</span>
        <input
          type="text"
          %{Vdom.Attr.string_property "value" value}
          %{input_style}
          on_input=%{fun (_ : _) v -> set v} />
      </label>
    |}
  in
  let params_card =
    let update f value = set_param_text (f param_text value) in
    let fill_fields =
      [ param_field
          ~label:"half spread $"
          ~value:param_text.Replay.Param_text.half_spread
          ~set:
            (update (fun p v -> { p with Replay.Param_text.half_spread = v }))
      ; param_field
          ~label:"participation cap"
          ~value:param_text.Replay.Param_text.participation
          ~set:
            (update (fun p v ->
               { p with Replay.Param_text.participation = v }))
      ; param_field
          ~label:"impact coeff $"
          ~value:param_text.Replay.Param_text.impact
          ~set:(update (fun p v -> { p with Replay.Param_text.impact = v }))
      ]
    in
    let algo_fields =
      if String.equal algo "pov"
      then
        [ param_field
            ~label:"participation rate"
            ~value:param_text.Replay.Param_text.pov_rate
            ~set:
              (update (fun p v -> { p with Replay.Param_text.pov_rate = v }))
        ]
      else if String.equal algo "is"
      then
        [ param_field
            ~label:"urgency (0 = TWAP)"
            ~value:param_text.Replay.Param_text.urgency
            ~set:
              (update (fun p v -> { p with Replay.Param_text.urgency = v }))
        ]
      else []
    in
    let engine_pills =
      let pill value label =
        algo_pill
          ~theme
          ~selected:(String.equal param_text.Replay.Param_text.engine value)
          ~on_click:(fun _ ->
            update (fun p v -> { p with Replay.Param_text.engine = v }) value)
          label
      in
      [ {%html|
          <div %{Styles.s "display:flex;gap:8px;align-items:flex-end;"}>
            %{pill "bar" "Bar model"}
            %{pill "synthetic" "Synthetic exchange"}
          </div>
        |}
      ]
      @
      if String.equal param_text.Replay.Param_text.engine "synthetic"
      then
        [ param_field
            ~label:"seed"
            ~value:param_text.Replay.Param_text.seed
            ~set:(update (fun p v -> { p with Replay.Param_text.seed = v }))
        ]
      else []
    in
    {%html|
      <div %{Styles.card theme "padding:20px;"}>
        <div %{section_label}>Parameters</div>
        <div %{Styles.s "display:flex;gap:14px;flex-wrap:wrap;"}>
          *{fill_fields}
          *{algo_fields}
        </div>
        <div %{Styles.s (Styles.label theme ^ "margin:14px 0 8px 0;")}>
          Fill engine
        </div>
        <div %{Styles.s "display:flex;gap:14px;flex-wrap:wrap;"}>
          *{engine_pills}
        </div>
      </div>
    |}
  in
  {%html|
    <div class="page fade" %{Styles.s narrow_page}>
      %{wizard_header ~step:2 ~theme ~is_dark ~toggle_theme
          ~title:"New simulation" ~subtitle
          ~back:None ()}
      <div %{Styles.s two_col}>
        <div %{Styles.card theme "padding:20px;"}>
          <div %{section_label}>Execution algorithm</div>
          <div %{pills}>
            %{algo_pill ~theme ~selected:(String.equal algo "twap")
                ~on_click:(fun _ -> set_algo "twap") "TWAP"}
            %{algo_pill ~theme ~selected:(String.equal algo "vwap")
                ~on_click:(fun _ -> set_algo "vwap") "VWAP"}
            %{algo_pill ~theme ~selected:(String.equal algo "pov")
                ~on_click:(fun _ -> set_algo "pov") "POV"}
            %{algo_pill ~theme ~selected:(String.equal algo "is")
                ~on_click:(fun _ -> set_algo "is") "IS"}
            %{algo_pill ~theme ~selected:(String.equal algo "immediate")
                ~on_click:(fun _ -> set_algo "immediate") "Immediate"}
          </div>
        </div>
        <div %{Styles.card theme "padding:20px;"}>
          <div %{section_label}>Alpha instructions</div>
          *{instructions}
        </div>
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
  ~runs
  ~to_sim
  ~new_sim
  ~retest
  ~to_dashboard
  ~toggle_theme
  ~player
  ~set_player
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
  let capture =
    if total_gross > 0
    then sprintf "%.1f%%" (total_net // total_gross *. 100.)
    else "n/a"
  in
  let title =
    sprintf
      "Results — %s · %s · %s"
      (Symbol.to_string replay.symbol)
      (Date.to_string replay.date)
      (String.uppercase replay.algo_name)
  in
  (* The layman's scoreboard: one verdict sentence, then four big tiles with
     the technical term demoted to a footnote. *)
  let summary =
    let value_add = replay.results.total_value_add_cents in
    let verdict =
      if value_add > 0
      then
        sprintf
          "Patient execution kept %s more of your alpha than dumping every \
           order instantly."
          (dollars_signed value_add)
      else if value_add < 0
      then
        sprintf
          "On this day, instant execution would have done better — patience \
           cost %s."
          (dollars_signed (-value_add))
      else "Execution matched the instant-trading baseline exactly."
    in
    let money_color cents =
      if cents > 0
      then theme.Styles.green
      else if cents < 0
      then theme.Styles.red
      else theme.Styles.secondary
    in
    let tile ~label ~sub ~color value =
      let tile_style =
        Styles.s
          ("display:flex;flex-direction:column;gap:2px;background:"
           ^ theme.Styles.chip_bg
           ^ ";border-radius:8px;padding:14px 16px;")
      in
      let value_style =
        Styles.s
          ("font-size:22px;font-weight:700;color:"
           ^ color
           ^ ";"
           ^ Styles.mono)
      in
      let sub_style =
        Styles.s ("font-size:11.5px;color:" ^ theme.Styles.faint ^ ";")
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
        ("font-size:15px;font-weight:600;color:"
         ^ theme.Styles.text
         ^ ";margin-bottom:12px;")
    in
    let tiles =
      Styles.s
        "display:grid;grid-template-columns:repeat(auto-fit,minmax(200px,1fr));gap:10px;"
    in
    let help_button =
      let style =
        Styles.s
          ("display:inline-flex;align-items:center;justify-content:center;width:26px;height:26px;border-radius:999px;background:"
           ^ theme.Styles.chip_bg
           ^ ";color:"
           ^ theme.Styles.blue
           ^ ";border:1px solid "
           ^ theme.Styles.chip_border
           ^ ";cursor:pointer;font-size:14px;font-weight:800;flex-shrink:0;"
          )
      in
      {%html|
        <button
          class="btn"
          %{style}
          title="What do these numbers mean?"
          on_click=%{fun _ -> open_help}>
          ?
        </button>
      |}
    in
    let verdict_row =
      Styles.s
        "display:flex;justify-content:space-between;align-items:flex-start;gap:14px;margin-bottom:12px;"
    in
    {%html|
      <div %{Styles.card theme "padding:20px;"}>
        <div %{verdict_row}>
          <span %{verdict_style}>#{verdict}</span>
          %{help_button}
        </div>
        <div %{tiles}>
          %{tile ~label:"Your alpha predicted"
              ~sub:"profit if every order filled instantly and free"
              ~color:(money_color total_gross)
              (dollars_signed total_gross)}
          %{tile ~label:"You actually kept"
              ~sub:"net P&L after realistic trading costs"
              ~color:(money_color total_net)
              (dollars_signed total_net)}
          %{tile ~label:"Alpha captured"
              ~sub:"the share of the prediction that survived"
              ~color:theme.Styles.text capture}
          %{tile ~label:"Execution bonus"
              ~sub:"vs selling/buying everything the moment it arrived"
              ~color:(money_color value_add)
              (dollars_signed value_add)}
        </div>
      </div>
    |}
  in
  (* Same-day leaderboard: the best stored run of each algorithm on this
     symbol+session, ranked by execution bonus. *)
  let leaderboard =
    let entries =
      List.filter runs ~f:(fun (run : History.Run_record.t) ->
        Symbol.equal run.symbol replay.symbol
        && Date.equal run.date replay.date)
      |> List.sort_and_group ~compare:(fun (a : History.Run_record.t) b ->
        String.compare a.algo_name b.algo_name)
      |> List.filter_map ~f:(fun group ->
        List.max_elt group ~compare:(fun (a : History.Run_record.t) b ->
          Int.compare a.value_add_cents b.value_add_cents))
      |> List.sort ~compare:(fun (a : History.Run_record.t) b ->
        Int.compare b.value_add_cents a.value_add_cents)
    in
    let max_abs =
      List.fold entries ~init:1 ~f:(fun acc run ->
        Int.max acc (Int.abs run.value_add_cents))
    in
    let badge_color rank =
      match rank with
      | 0 -> "#d4a417"
      | 1 -> "#8f98a3"
      | 2 -> "#b0764a"
      | _ -> theme.Styles.faint
    in
    let entry_row rank (run : History.Run_record.t) =
      let is_current = String.equal run.algo_name replay.algo_name in
      let row =
        Styles.s
          ("display:grid;grid-template-columns:36px 110px 70px 1fr \
            120px;column-gap:12px;align-items:center;padding:9px \
            6px;border-bottom:1px solid "
           ^ theme.Styles.hairline
           ^ ";border-radius:6px;background:"
           ^ (if is_current then theme.Styles.blue_soft else "transparent")
           ^ ";")
      in
      let badge =
        Styles.s
          ("width:26px;height:26px;border-radius:999px;display:flex;align-items:center;justify-content:center;background:"
           ^ badge_color rank
           ^ ";color:#ffffff;font-size:12px;font-weight:800;")
      in
      let algo_style =
        Styles.s
          ("font-size:13px;font-weight:700;color:"
           ^ (if is_current then theme.Styles.blue else theme.Styles.text)
           ^ ";")
      in
      let capture_style =
        Styles.s
          ("font-size:12px;color:"
           ^ theme.Styles.secondary
           ^ ";"
           ^ Styles.mono)
      in
      let capture_text =
        match run.alpha_capture with
        | None -> "—"
        | Some c -> sprintf "%.1f%%" (c *. 100.)
      in
      let track =
        Styles.s
          ("height:8px;border-radius:999px;background:"
           ^ theme.Styles.chip_bg
           ^ ";overflow:hidden;")
      in
      let bar_width = 100 * Int.abs run.value_add_cents / max_abs in
      let bar =
        Styles.s
          (sprintf
             "height:100%%;width:%d%%;border-radius:999px;background:%s;"
             (Int.max 2 bar_width)
             (if run.value_add_cents >= 0
              then theme.Styles.green
              else theme.Styles.red))
      in
      let this_run =
        if is_current
        then
          [ {%html|
              <span
                %{Styles.s
                    ("font-size:10.5px;font-weight:700;color:#ffffff;background:"
                     ^ theme.Styles.blue
                     ^ ";border-radius:999px;padding:2px 8px;margin-left:6px;")}>
                this run
              </span>
            |}
          ]
        else []
      in
      {%html|
        <div %{row}>
          <span %{badge}>#{Int.to_string (rank + 1)}</span>
          <span %{algo_style}>
            #{String.uppercase run.algo_name}
            *{this_run}
          </span>
          <span %{capture_style}>#{capture_text}</span>
          <div %{track}><div class="grow-x" %{bar}></div></div>
          <span %{Styles.s "text-align:right;"}>
            %{pnl_cell ~theme run.value_add_cents}
          </span>
        </div>
      |}
    in
    let body =
      match entries with
      | [] | [ _ ] ->
        [ {%html|
            <div
              %{Styles.s
                  ("font-size:13px;color:" ^ theme.Styles.secondary ^ ";padding:6px 0;")}>
              Retest this day with a different algorithm and the ranking
              fills in — same alpha, same market, only the execution
              changes.
            </div>
          |}
        ]
      | entries -> List.mapi entries ~f:entry_row
    in
    let head =
      Styles.s
        "display:flex;justify-content:space-between;align-items:center;margin-bottom:8px;"
    in
    {%html|
      <div %{Styles.card theme "padding:20px;"}>
        <div %{head}>
          <span
            %{Styles.s
                ("color:"
                 ^ theme.Styles.text
                 ^ ";font-size:14px;font-weight:600;")}>
            Leaderboard — this day, algorithm vs algorithm
          </span>
          %{secondary_button ~theme ~on_click:(fun _ -> retest)
              "Try another algorithm"}
        </div>
        *{body}
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
  let cost_columns = "56px 112px 104px 116px 104px 104px 128px 1fr" in
  let cost_row index (row : Replay.result_row) =
    let grading = row.Replay.grading in
    let avg_fill, shortfall_bps =
      match grading.Transaction_cost.fill_metrics with
      | None -> dash, dash
      | Some metrics ->
        ( {%html|<span>#{sprintf "$%.4f" metrics.average_fill_price}</span>|}
        , bps_view ~theme metrics.shortfall_bps )
    in
    {%html|
      <div %{body_row cost_columns}>
        %{order_cell index}
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
    "56px 112px 64px 122px 118px 118px 130px 130px 1fr"
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
    let name_input =
      let input_style =
        Styles.s
          ("width:140px;background:"
           ^ theme.Styles.page_bg
           ^ ";color:"
           ^ theme.Styles.text
           ^ ";border:1px solid "
           ^ theme.Styles.chip_border
           ^ ";border-radius:4px;padding:6px 8px;font-size:12.5px;"
           ^ Styles.mono)
      in
      {%html|
        <input
          type="text"
          placeholder="your name"
          %{Vdom.Attr.string_property "value" player}
          %{input_style}
          on_input=%{fun (_ : _) name -> set_player name} />
      |}
    in
    let submit_button =
      let style =
        Styles.s
          ("background:"
           ^ theme.Styles.blue
           ^ ";color:#ffffff;border:none;border-radius:5px;padding:7px \
              14px;cursor:pointer;font-size:12.5px;font-weight:700;")
      in
      {%html|
        <button class="btn" %{style} on_click=%{fun _ -> submit}>
          Submit to leaderboard
        </button>
      |}
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
    let columns = "44px 160px 90px 130px 130px 80px 1fr" in
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
              <span %{dim}>#{String.uppercase row.algo_name}</span>
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
          <span>algo</span>
          <span>vs immediate</span>
          <span>net P&L</span>
          <span>capture</span>
          <span>submitted</span>
        </div>
        *{board_rows}
      </div>
    |}
  in
  let buttons = Styles.s "display:flex;gap:10px;align-items:center;" in
  let export_name suffix =
    sprintf
      "execlab_%s_%s_%s_%s.csv"
      (Symbol.to_string replay.symbol)
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
        ("background:"
         ^ theme.Styles.chip_bg
         ^ ";color:"
         ^ theme.Styles.secondary
         ^ ";border:1px solid "
         ^ theme.Styles.chip_border
         ^ ";border-radius:5px;padding:9px \
            16px;cursor:pointer;font-size:13px;font-weight:600;")
    in
    {%html|<button %{style} on_click=%{on_click}>#{label}</button>|}
  in
  let page =
    Styles.s
      "display:flex;flex-direction:column;gap:16px;max-width:1240px;margin:0 \
       auto;padding:28px 20px;"
  in
  {%html|
    <div class="page fade" %{page}>
      %{wizard_header ~step:4 ~theme ~is_dark ~toggle_theme ~title
          ~subtitle:"shortfall split into the metric tree: timing + spread \
                     + impact, plus opportunity on unfilled shares"
          ~back:(Some ("← Replay", to_sim)) ()}
      %{summary}
      <div %{Styles.card theme "padding-bottom:4px;"}>
        <div %{Styles.s "padding:14px 16px 0 16px;"}>
          <span %{title_style}>Execution cost breakdown</span>
        </div>
        <div %{head_row cost_columns}>
          <span>order</span>
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
      <div %{Styles.card theme "padding-bottom:4px;"}>
        <div %{Styles.s "padding:14px 16px 0 16px;"}>
          <span %{title_style}>Results</span>
        </div>
        <div %{head_row results_columns}>
          <span>order</span>
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
      %{leaderboard}
      %{leaderboard_card}
      <div %{buttons}>
        %{primary_button ~theme ~on_click:(fun _ -> new_sim)
            "New simulation"}
        %{primary_button ~theme ~on_click:(fun _ -> to_dashboard)
            "Dashboard"}
        %{ghost_button
            ~on_click:(fun _ ->
              download (export_name "results") (Replay.results_csv replay))
            "⤓ Results CSV"}
        %{ghost_button
            ~on_click:(fun _ ->
              download (export_name "fills") (Replay.fills_csv replay))
            "⤓ Fills CSV"}
      </div>
    </div>
  |}
;;

(* ---------- the leaderboard seam ---------- *)

let submit_run_effect = Effect.of_deferred_fun Net.submit_run
let fetch_board_effect = Effect.of_deferred_fun Net.leaderboard

let persist_player =
  Effect.of_sync_fun (fun name -> Storage.set Storage.player_key name)
;;

let config_of (replay : Replay.t) ~player =
  let fill = replay.params.Execlab_session.Params.fill_config in
  { Run_config.player
  ; symbol = replay.symbol
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
let run_record (replay : Replay.t) =
  let rows = replay.results.rows in
  let sum f = List.sum (module Int) rows ~f in
  let net =
    sum (fun row -> row.Replay.grading.Transaction_cost.net_pnl_cents)
  in
  let gross =
    sum (fun row -> row.Replay.grading.gross_theoretical_pnl_cents)
  in
  { History.Run_record.symbol = replay.symbol
  ; date = replay.date
  ; algo_name = replay.algo_name
  ; alpha_capture = (if gross > 0 then Some (net // gross) else None)
  ; value_add_cents = replay.results.total_value_add_cents
  ; net_cents = net
  }
;;

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
  let player, set_player =
    Bonsai.state
      (Option.value (Storage.get Storage.player_key) ~default:"")
      graph
  in
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
  let show_fills, set_show_fills = Bonsai.state false graph in
  let param_text, set_param_text =
    Bonsai.state Replay.Param_text.default graph
  in
  let hover, set_hover = Bonsai.state (None : int option) graph in
  (* Chart zoom: [None] = full session, [Some span] = span minutes centered
     on the playhead. *)
  let zoom, set_zoom = Bonsai.state (None : int option) graph in
  let is_dark, set_is_dark = Bonsai.state false graph in
  (* The symbol whose calendar the choose-day screen is browsing; distinct
     from [selection], which is only set once a session is clicked. *)
  let cal_symbol, set_cal_symbol =
    Bonsai.state (None : Symbol.t option) graph
  in
  let advance =
    let%arr playing and speed and replay and set_minute in
    match replay with
    | Some r when playing ->
      set_minute (fun m -> Int.min (Replay.last_minute r) (m + speed))
    | Some _ | None -> Effect.Ignore
  in
  Bonsai.Clock.every
    ~when_to_start_next_effect:`Every_multiple_of_period_non_blocking
    ~trigger_on_activate:false
    (Bonsai.return (Time_ns.Span.of_ms 250.))
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
    and set_submit_status in
    match selection, Replay.parse_params param_text with
    | None, _ -> set_run_error (Some (Error.of_string "choose a day first"))
    | Some (_ : Symbol.t * Date.t), Error error -> set_run_error (Some error)
    | Some (symbol, date), Ok params ->
      let%bind.Effect result =
        Effect.of_sync_fun
          (fun () ->
            Replay.run ~symbol ~date ~alpha_text ~algo_name:algo ~params)
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
         let%bind.Effect () = set_runs (History.add (run_record r) runs) in
         set_screen Screen.Sim)
  in
  let restart =
    let%arr set_minute and set_playing in
    let%bind.Effect () = set_minute (fun (_ : int) -> 0) in
    set_playing true
  in
  let submit =
    let%arr replay and player and set_board and set_submit_status in
    match replay with
    | None -> Effect.Ignore
    | Some r ->
      let%bind.Effect () = set_submit_status (Some "submitting…") in
      let%bind.Effect response = submit_run_effect (config_of r ~player) in
      (match response with
       | Ok resp ->
         let%bind.Effect () =
           set_board (Some resp.Submit_run.Response.leaderboard)
         in
         set_submit_status (Some "verified by the server ✓")
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
          { Leaderboard.Request.symbol = r.symbol
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
  and zoom
  and set_zoom
  and is_dark
  and set_is_dark
  and cal_symbol
  and set_cal_symbol
  and set_minute
  and set_screen
  and start
  and restart
  and player
  and set_player
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
  let dashboard () =
    dashboard_view
      ~theme
      ~is_dark
      ~runs
      ~new_sim:(goto Screen.Choose_day)
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
        ~to_dashboard:(goto Screen.Dashboard)
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
        ~toggle_theme
        ~back:(goto Screen.Alpha)
    | Sim, Some r, Some (_ : Symbol.t * Date.t) ->
      sim_view
        r
        ~theme
        ~is_dark
        ~minute
        ~playing
        ~speed
        ~show_fills
        ~zoom
        ~set_playing
        ~set_speed
        ~set_zoom
        ~set_minute
        ~restart
        ~toggle_fills:(set_show_fills (not show_fills))
        ~toggle_theme
        ~to_results:(goto Screen.Results)
        ~back:(goto Screen.Setup)
        ~hover
        ~set_hover
    | Results, Some r, Some (_ : Symbol.t * Date.t) ->
      results_view
        r
        ~theme
        ~is_dark
        ~open_help:(set_show_help true)
        ~runs
        ~to_sim:(goto Screen.Sim)
        ~new_sim:(goto Screen.Choose_day)
        ~retest:(goto Screen.Setup)
        ~to_dashboard:(goto Screen.Dashboard)
        ~toggle_theme
        ~player
        ~set_player:(fun name ->
          let%bind.Effect () = set_player name in
          persist_player name)
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
