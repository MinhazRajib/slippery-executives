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
  ~n
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
             0
             (Int.min
                shown
                (Float.iround_nearest_exn (ratio *. Float.of_int (n - 1))))))
;;

let chart
  (replay : Replay.t)
  ~theme
  ~minute
  ~fills
  ~show_fills
  ~hover
  ~set_hover
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
  let lo =
    Array.fold bars ~init:Float.infinity ~f:(fun acc bar ->
      Float.min acc (Price.to_float bar.Market_bar.low))
  in
  let hi =
    Array.fold bars ~init:Float.neg_infinity ~f:(fun acc bar ->
      Float.max acc (Price.to_float bar.Market_bar.high))
  in
  let span = Float.max (hi -. lo) 0.01 in
  let x i = left +. (Float.of_int i /. Float.of_int (n - 1) *. plot_w) in
  let y v = top +. ((hi -. v) /. span *. price_h) in
  let svg name attrs children = Vdom.Node.create_svg name ~attrs children in
  let attr = Vdom.Attr.create in
  let tooltip text = svg "title" [] [ Vdom.Node.text text ] in
  let hover = Option.map hover ~f:(fun m -> Int.max 0 (Int.min m minute)) in
  let hovered_in (parent : Replay.parent_replay) =
    match hover with
    | None -> false
    | Some m -> m >= parent.arrival_minute && m <= parent.deadline_minute
  in
  (* horizontal gridlines at round dollar levels *)
  let grid =
    let step = Float.max 1. (Float.round_up (span /. 5.)) in
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
          [ Vdom.Node.text (sprintf "$%.0f" v) ]
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
  let windows =
    if compact_windows
    then
      List.mapi replay.parents ~f:(fun index parent ->
        let x0 = x parent.arrival_minute in
        let x1 = x parent.deadline_minute in
        svg
          "rect"
          [ attr "x" (fs x0)
          ; attr "y" (fs (top +. 4. +. (Float.of_int index *. 8.)))
          ; attr "width" (fs (Float.max 3. (x1 -. x0)))
          ; attr "height" (if hovered_in parent then "6" else "4")
          ; attr "rx" "2"
          ; attr "fill" (Styles.order_color theme index)
          ; attr "fill-opacity" (if hovered_in parent then "1" else "0.9")
          ]
          [ window_tooltip index parent ])
    else
      List.concat_mapi replay.parents ~f:(fun index parent ->
        let color = Styles.order_color theme index in
        let x0 = x parent.arrival_minute in
        let x1 = x parent.deadline_minute in
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
        ])
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
  (* price line up to the playhead, with an end dot *)
  let price_line =
    let pts =
      List.init (minute + 1) ~f:(fun i ->
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
        [ attr "cx" (fs (x minute))
        ; attr "cy" (fs (y (Price.to_float bars.(minute).Market_bar.close)))
        ; attr "r" "4"
        ; attr "fill" theme.Styles.blue
        ]
        []
    ]
  in
  let time_axis =
    List.filter_map (List.init n ~f:Fn.id) ~f:(fun i ->
      if i % 60 = 0 && i > 0
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
      List.map fills ~f:(fun (fill : Fill.t) ->
        let index = Replay.parent_index_of_order replay fill.order_id in
        let m = Replay.minute_of_time replay fill.time in
        svg
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
          ])
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
        set_hover (minute_of_mouse ~n ~shown:minute evt))
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

let sim_view
  (replay : Replay.t)
  ~theme
  ~is_dark
  ~minute
  ~playing
  ~speed
  ~show_fills
  ~set_playing
  ~set_speed
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
  let back_style =
    Styles.s
      ("background:none;border:none;color:"
       ^ theme.Styles.blue
       ^ ";cursor:pointer;font-size:13px;font-weight:600;padding:0;")
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
            <button %{back_style} on_click=%{fun _ -> back}>
              ← New simulation
            </button>
            <button %{back_style} on_click=%{fun _ -> to_results}>
              Results →
            </button>
          </span>
        </div>
        <div %{title_style}>#{title}</div>
        <div %{sub_style}>
          source: <span %{Styles.code_chip theme}>#{command}</span>
        </div>
      </div>
      %{controls replay ~theme ~minute ~playing ~speed ~set_playing
          ~set_speed ~set_minute ~restart}
      <div %{Styles.card theme "padding-bottom:8px;"}>
        %{legend replay ~theme ~minute ~fills ~show_fills ~toggle_fills}
        %{chart replay ~theme ~minute ~fills ~show_fills ~hover
            ~set_hover}
      </div>
      %{orders_table replay ~theme ~fills ~minute}
      %{event_log replay ~theme ~fills ~minute}
    </div>
  |}
;;

(* Small inline SVG glyphs (Lucide outlines), stroke = currentColor so they
   inherit the surrounding text color in either theme. *)
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

let algo_pill ~theme ~selected ~on_click label =
  let bg = if selected then theme.Styles.blue else theme.Styles.chip_bg in
  let color = if selected then "#ffffff" else theme.Styles.secondary in
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
      | `Active -> theme.Styles.blue, "#ffffff", "700"
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

let primary_button ?(enabled = true) ?icon ~theme ~on_click label =
  let style =
    Styles.s
      ("display:inline-flex;align-items:center;gap:8px;background:"
       ^ theme.Styles.blue
       ^ ";color:#ffffff;border:none;border-radius:8px;padding:13px \
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

let narrow_page =
  "display:flex;flex-direction:column;gap:16px;max-width:1240px;margin:32px \
   auto;padding:20px;"
;;

(* Side-by-side halves for the wizard screens; collapses on narrow windows. *)
let two_col =
  "display:grid;grid-template-columns:repeat(auto-fit,minmax(420px,1fr));gap:16px;align-items:start;"
;;

(* ---------- dashboard ---------- *)

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
    "width:46px;height:34px;display:flex;align-items:center;justify-content:center;border-radius:5px;font-size:12.5px;"
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
        let color = if selected then "#ffffff" else theme.Styles.text in
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
            class="btn"
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
      Styles.s "display:grid;grid-template-columns:repeat(5,46px);gap:4px;"
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
  let section_label = Styles.s (Styles.label theme ^ "margin-bottom:8px;") in
  let summary_rows =
    let row_base =
      "display:grid;grid-template-columns:110px 1fr 1fr 1fr \
       1fr;column-gap:10px;align-items:baseline;"
    in
    let head =
      Styles.s
        (row_base
         ^ "padding:8px 0 6px 0;border-bottom:1px solid "
         ^ theme.Styles.hairline
         ^ ";"
         ^ Styles.label theme)
    in
    let num = "text-align:right;" in
    let summary_row (s : Dataset.Day_summary.t) =
      let selected =
        match selection with
        | Some (sym, d) ->
          Symbol.equal sym browse_symbol && Date.equal d s.date
        | None -> false
      in
      let style =
        Styles.s
          (row_base
           ^ "padding:7px 6px;font-size:13px;border-bottom:1px solid "
           ^ theme.Styles.hairline
           ^ ";cursor:pointer;border-radius:4px;background:"
           ^ (if selected then theme.Styles.blue_soft else "transparent")
           ^ ";color:"
           ^ theme.Styles.text
           ^ ";"
           ^ Styles.mono)
      in
      let hover_class =
        if selected then Vdom.Attr.empty else Vdom.Attr.class_ "hoverable"
      in
      let cell = Styles.s num in
      {%html|
        <div
          %{style}
          %{hover_class}
          on_click=%{fun _ -> select browse_symbol s.date}>
          <span>#{Date.to_string s.date}</span>
          <span %{cell}>#{sprintf "%.2f" s.open_}</span>
          <span %{cell}>#{sprintf "%.2f" s.close}</span>
          <span %{cell}>#{sprintf "%+.0f bps" s.return_bps}</span>
          <span %{cell}>#{Float.to_string_hum ~delimiter:',' ~decimals:0
                            (Float.of_int s.volume)}</span>
        </div>
      |}
    in
    {%html|
      <div %{head}>
        <span>session</span>
        <span %{Styles.s num}>open</span>
        <span %{Styles.s num}>close</span>
        <span %{Styles.s num}>day move</span>
        <span %{Styles.s num}>volume</span>
      </div>
    |}
    :: List.map (Dataset.summaries_for browse_symbol) ~f:summary_row
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
        <div %{Styles.card theme "padding:20px;"}>
          <div %{section_label}>Sessions — click to select</div>
          *{summary_rows}
        </div>
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
  [ ( "Demo mix"
    , csv
        [ "10:00:00", "BUY", 5000, "11:00:00"
        ; "11:30:00", "SELL", 3000, "13:00:00"
        ; "14:00:00", "BUY", 2000, "14:30:00"
        ] )
  ; ( "Morning accumulation"
    , csv
        [ "09:45:00", "BUY", 3000, "11:00:00"
        ; "10:15:00", "BUY", 3000, "12:00:00"
        ; "11:00:00", "BUY", 2000, "12:30:00"
        ] )
  ; ( "Round trip"
    , csv
        [ "10:00:00", "BUY", 6000, "11:30:00"
        ; "13:00:00", "SELL", 6000, "15:30:00"
        ] )
  ; ( "Afternoon liquidation"
    , csv
        [ "13:00:00", "SELL", 2500, "14:30:00"
        ; "13:45:00", "SELL", 2500, "15:00:00"
        ; "14:30:00", "SELL", 3000, "15:55:00"
        ] )
  ; ( "Busy tape"
    , csv
        [ "09:40:00", "BUY", 1500, "10:30:00"
        ; "10:20:00", "SELL", 1000, "11:15:00"
        ; "11:00:00", "BUY", 2000, "12:30:00"
        ; "12:15:00", "SELL", 1500, "13:45:00"
        ; "13:30:00", "BUY", 1000, "14:30:00"
        ; "14:45:00", "SELL", 2000, "15:45:00"
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
  let sample_pill (name, csv) =
    let style =
      Styles.s
        ("background:"
         ^ theme.Styles.chip_bg
         ^ ";color:"
         ^ theme.Styles.secondary
         ^ ";border:1px solid "
         ^ theme.Styles.chip_border
         ^ ";border-radius:4px;padding:4px \
            10px;cursor:pointer;font-size:12px;font-weight:600;")
    in
    {%html|
      <button %{style} on_click=%{fun _ -> set_alpha_text csv}>
        #{name}
      </button>
    |}
  in
  let samples_row =
    Styles.s "display:flex;gap:6px;flex-wrap:wrap;margin-bottom:10px;"
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
          <div %{samples_row}>
            *{List.map (sample_alphas symbol) ~f:sample_pill}
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
            %{algo_pill ~theme ~selected:(String.equal algo "immediate")
                ~on_click:(fun _ -> set_algo "immediate") "Immediate"}
          </div>
        </div>
        <div %{Styles.card theme "padding:20px;"}>
          <div %{section_label}>Alpha instructions</div>
          *{instructions}
        </div>
      </div>
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
  ~to_sim
  ~new_sim
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
  let summary =
    let strip =
      Styles.s
        "display:flex;gap:24px;align-items:baseline;padding:14px 16px;"
    in
    let label_style = Styles.s (Styles.label theme) in
    let capture_style =
      Styles.s
        ("color:"
         ^ theme.Styles.text
         ^ ";font-size:13px;font-weight:600;"
         ^ Styles.mono)
    in
    {%html|
      <div %{Styles.card theme ""}>
        <div %{strip}>
          %{money_stat ~theme ~label:"Execution benefit vs immediate"
              replay.results.total_value_add_cents}
          %{money_stat ~theme ~label:"Net P&L" total_net}
          %{money_stat ~theme ~label:"Gross alpha" total_gross}
          <span>
            <span %{label_style}>Alpha captured </span>
            <span %{capture_style}>#{capture}</span>
          </span>
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
    <div %{page}>
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
  let screen, set_screen = Bonsai.state Screen.Dashboard graph in
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
  let replay, set_replay = Bonsai.state (None : Replay.t option) graph in
  let minute, set_minute = Bonsai.state' 0 graph in
  let playing, set_playing = Bonsai.state true graph in
  let speed, set_speed = Bonsai.state 4 graph in
  let show_fills, set_show_fills = Bonsai.state false graph in
  let hover, set_hover = Bonsai.state (None : int option) graph in
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
    and runs
    and set_runs
    and set_run_error
    and set_replay
    and set_screen
    and set_minute
    and set_playing in
    match selection with
    | None -> set_run_error (Some (Error.of_string "choose a day first"))
    | Some (symbol, date) ->
      let%bind.Effect result =
        Effect.of_sync_fun
          (fun () -> Replay.run ~symbol ~date ~alpha_text ~algo_name:algo)
          ()
      in
      (match result with
       | Error error -> set_run_error (Some error)
       | Ok r ->
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
  let%arr screen
  and selection
  and set_selection
  and alpha_text
  and set_alpha_text
  and algo
  and set_algo
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
  and hover
  and set_hover
  and is_dark
  and set_is_dark
  and cal_symbol
  and set_cal_symbol
  and set_minute
  and set_screen
  and start
  and restart in
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
        ~set_playing
        ~set_speed
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
        ~to_sim:(goto Screen.Sim)
        ~new_sim:(goto Screen.Choose_day)
        ~to_dashboard:(goto Screen.Dashboard)
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
  {%html|<div %{shell}>%{body}</div>|}
;;

let () = Bonsai_web.Start.start app
