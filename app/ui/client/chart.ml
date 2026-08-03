(* The session chart: the day's 390 one-minute closes as a line, a volume
   underlay, optional buy/sell fill markers, and a crosshair tooltip
   following the mouse. Used by the choose-a-day preview (no fills) and the
   results screen (with the run's fills). Pure SVG via
   [Vdom.Node.create_svg]; colors and text styling come from the [.chart-*]
   classes in [index.html]. *)

open! Core
open! Execlab_types
open! Execlab_market
open! Bonsai_web
open Bonsai.Let_syntax

let view_w = 900.
let view_h = 300.
let margin_left = 10.
let margin_right = 62.
let margin_top = 14.
let margin_bottom = 26.
let plot_w = view_w -. margin_left -. margin_right
let plot_h = view_h -. margin_top -. margin_bottom

(* Bottom fraction of the plot given to the volume underlay; the price line
   keeps clear of it. *)
let volume_band = 0.18

let minute_of_ofday ofday =
  let session_open = Time_ns.Ofday.create ~hr:9 ~min:30 () in
  Float.to_int (Time_ns.Span.to_min (Time_ns.Ofday.diff ofday session_open))
;;

module Scale = struct
  type t =
    { bar_count : int
    ; price_lo : float
    ; price_hi : float
    ; max_volume : float
    }

  (* Fill prices can poke past the close range (spread + impact around the
     open), so the y-range covers both. *)
  let create (day : Trading_day.t) (fills : Fill.t list) =
    let closes =
      List.map day.bars ~f:(fun bar -> Price.to_float bar.close)
    in
    let fill_prices =
      List.map fills ~f:(fun fill -> Price.to_float fill.price)
    in
    let prices = closes @ fill_prices in
    let lo =
      List.min_elt prices ~compare:Float.compare |> Option.value ~default:0.
    in
    let hi =
      List.max_elt prices ~compare:Float.compare |> Option.value ~default:1.
    in
    let pad = Float.max ((hi -. lo) *. 0.06) 0.01 in
    let max_volume =
      List.map day.bars ~f:(fun bar -> Size.to_float bar.volume)
      |> List.max_elt ~compare:Float.compare
      |> Option.value ~default:1.
    in
    { bar_count = List.length day.bars
    ; price_lo = lo -. pad
    ; price_hi = hi +. pad
    ; max_volume = Float.max max_volume 1.
    }
  ;;

  let x t index =
    margin_left
    +. (plot_w *. Float.of_int index /. Float.of_int (t.bar_count - 1))
  ;;

  let y t price =
    let price_h = plot_h *. (1. -. volume_band) in
    margin_top
    +. ((1. -. ((price -. t.price_lo) /. (t.price_hi -. t.price_lo)))
        *. price_h)
  ;;

  let volume_height t volume =
    plot_h *. volume_band *. (volume /. t.max_volume)
  ;;
end

let svg name ~attrs children = Vdom.Node.create_svg name ~attrs children
let attr = Vdom.Attr.create

let price_path scale (bars : Market_bar.t list) =
  List.mapi bars ~f:(fun i bar ->
    let cmd = if i = 0 then "M" else "L" in
    sprintf
      "%s%.1f %.1f"
      cmd
      (Scale.x scale i)
      (Scale.y scale (Price.to_float bar.close)))
  |> String.concat ~sep:" "
;;

let volume_path scale (bars : Market_bar.t list) =
  let bar_w = plot_w /. Float.of_int scale.Scale.bar_count *. 0.7 in
  let base_y = margin_top +. plot_h in
  List.mapi bars ~f:(fun i bar ->
    let h = Scale.volume_height scale (Size.to_float bar.volume) in
    if Float.(h <= 0.)
    then ""
    else
      sprintf
        "M%.1f %.1f h%.1f v%.1f h%.1f Z "
        (Scale.x scale i -. (bar_w /. 2.))
        base_y
        bar_w
        (-.h)
        (-.bar_w))
  |> String.concat
;;

(* 4 evenly spaced price gridlines with right-hand labels. *)
let y_axis scale =
  List.init 4 ~f:(fun i ->
    let price =
      scale.Scale.price_lo
      +. ((scale.Scale.price_hi -. scale.Scale.price_lo)
          *. Float.of_int i
          /. 3.)
    in
    let y = Scale.y scale price in
    [ svg
        "line"
        ~attrs:
          [ Vdom.Attr.class_ "chart-grid"
          ; attr "x1" (sprintf "%.1f" margin_left)
          ; attr "x2" (sprintf "%.1f" (margin_left +. plot_w))
          ; attr "y1" (sprintf "%.1f" y)
          ; attr "y2" (sprintf "%.1f" y)
          ]
        []
    ; svg
        "text"
        ~attrs:
          [ Vdom.Attr.class_ "chart-label"
          ; attr "x" (sprintf "%.1f" (margin_left +. plot_w +. 8.))
          ; attr "y" (sprintf "%.1f" (y +. 3.5))
          ]
        [ Vdom.Node.text (sprintf "$%.2f" price) ]
    ])
  |> List.concat
;;

(* Hourly time labels along the bottom: 10:00 .. 15:00. *)
let x_axis scale =
  List.filter_map [ 10; 11; 12; 13; 14; 15 ] ~f:(fun hour ->
    let minute = (hour * 60) - 570 in
    if minute < 0 || minute >= scale.Scale.bar_count
    then None
    else
      Some
        (svg
           "text"
           ~attrs:
             [ Vdom.Attr.classes [ "chart-label"; "chart-label-x" ]
             ; attr "x" (sprintf "%.1f" (Scale.x scale minute))
             ; attr "y" (sprintf "%.1f" (view_h -. 8.))
             ]
           [ Vdom.Node.text (sprintf "%d:00" hour) ]))
;;

(* Buy = triangle up (aqua), sell = triangle down (red) — shape carries the
   side as well as color, with a surface-colored ring for separation. *)
let marker scale (fill : Fill.t) =
  let cx = Scale.x scale (minute_of_ofday fill.time) in
  let cy = Scale.y scale (Price.to_float fill.price) in
  let r = 5.0 in
  let points =
    match fill.side with
    | Side.Buy ->
      sprintf
        "%.1f,%.1f %.1f,%.1f %.1f,%.1f"
        cx
        (cy -. r)
        (cx +. r)
        (cy +. r)
        (cx -. r)
        (cy +. r)
    | Side.Sell ->
      sprintf
        "%.1f,%.1f %.1f,%.1f %.1f,%.1f"
        cx
        (cy +. r)
        (cx +. r)
        (cy -. r)
        (cx -. r)
        (cy -. r)
  in
  let side_class =
    match fill.side with
    | Side.Buy -> "chart-mk-buy"
    | Side.Sell -> "chart-mk-sell"
  in
  svg
    "polygon"
    ~attrs:[ Vdom.Attr.class_ side_class; attr "points" points ]
    []
;;

let hover_index_of_event event ~bar_count =
  let open Js_of_ocaml in
  match Js.Opt.to_option event##.currentTarget with
  | None -> None
  | Some target ->
    let rect = target##getBoundingClientRect in
    let rect_left = Js.to_float rect##.left in
    let rect_width = Js.to_float rect##.width in
    if Float.(rect_width <= 0.)
    then None
    else (
      let client_x = Js.to_float event##.clientX in
      let view_x = (client_x -. rect_left) /. rect_width *. view_w in
      let fraction = (view_x -. margin_left) /. plot_w in
      let index =
        Float.to_int
          (Float.round_nearest (fraction *. Float.of_int (bar_count - 1)))
      in
      Some (Int.clamp_exn index ~min:0 ~max:(bar_count - 1)))
;;

let tooltip scale (day : Trading_day.t) ~fills_by_minute ~index =
  match List.nth day.bars index with
  | None -> Vdom.Node.none
  | Some bar ->
    let fill_lines =
      match Map.find fills_by_minute (minute_of_ofday bar.time) with
      | None -> []
      | Some fills ->
        List.map fills ~f:(fun (fill : Fill.t) ->
          let arrow =
            match fill.side with
            | Side.Buy -> "▲ BUY "
            | Side.Sell -> "▼ SELL"
          in
          [%string
            "%{arrow} %{Fmt.shares fill.size} @ %{Fmt.price fill.price}"])
    in
    let ohlc_line =
      [%string
        "O %{Fmt.price bar.open_}  H %{Fmt.price bar.high}  L %{Fmt.price \
         bar.low}"]
    in
    let headline =
      [%string
        "%{Fmt.ofday bar.time}  C %{Fmt.price bar.close}  V %{Fmt.shares \
         bar.volume}"]
    in
    let text =
      String.concat ~sep:"\n" ([ headline; ohlc_line ] @ fill_lines)
    in
    let x_fraction = Scale.x scale index /. view_w in
    let translate =
      if Float.(x_fraction > 0.55)
      then "translateX(-104%)"
      else "translateX(6%)"
    in
    let style =
      attr
        "style"
        (sprintf
           "left: %.1f%%; top: 6px; transform: %s"
           (x_fraction *. 100.)
           translate)
    in
    Vdom.Node.div
      ~attrs:[ Vdom.Attr.class_ "chart-tip"; style ]
      [ Vdom.Node.text text ]
;;

let crosshair scale (day : Trading_day.t) ~index =
  match List.nth day.bars index with
  | None -> []
  | Some bar ->
    let x = Scale.x scale index in
    let y = Scale.y scale (Price.to_float bar.close) in
    [ svg
        "line"
        ~attrs:
          [ Vdom.Attr.class_ "chart-crosshair"
          ; attr "x1" (sprintf "%.1f" x)
          ; attr "x2" (sprintf "%.1f" x)
          ; attr "y1" (sprintf "%.1f" margin_top)
          ; attr "y2" (sprintf "%.1f" (margin_top +. plot_h))
          ]
        []
    ; svg
        "circle"
        ~attrs:
          [ Vdom.Attr.class_ "chart-dot"
          ; attr "cx" (sprintf "%.1f" x)
          ; attr "cy" (sprintf "%.1f" y)
          ; attr "r" "3.5"
          ]
        []
    ]
;;

let empty_view =
  {%html|
    <div class="panel chart-empty">
      <span class="muted">Pick a symbol and a date to preview the session.</span>
    </div>
  |}
;;

let render
  (day : Trading_day.t)
  (fills : Fill.t list)
  ~visible_upto
  ~on_seek
  ~hover
  ~set_hover
  =
  (* Playback: scales come from the FULL day so nothing jumps as the clock
     advances, but the line, volume, fills, and hover stop at [visible_upto]
     — no peeking at the future mid-replay. *)
  let scale = Scale.create day fills in
  let visible_bars =
    match visible_upto with
    | None -> day.Trading_day.bars
    | Some minute -> List.take day.Trading_day.bars (minute + 1)
  in
  let fills =
    match visible_upto with
    | None -> fills
    | Some minute ->
      List.filter fills ~f:(fun fill ->
        minute_of_ofday fill.Fill.time <= minute)
  in
  let clamp_hover index =
    match visible_upto with
    | None -> index
    | Some minute -> Int.min index minute
  in
  let fills_by_minute =
    List.fold fills ~init:Int.Map.empty ~f:(fun map (fill : Fill.t) ->
      Map.add_multi map ~key:(minute_of_ofday fill.time) ~data:fill)
    |> Map.map ~f:List.rev
  in
  let now_line =
    match visible_upto with
    | None -> []
    | Some minute ->
      let x = Scale.x scale minute in
      [ svg
          "line"
          ~attrs:
            [ Vdom.Attr.class_ "chart-now"
            ; attr "x1" (sprintf "%.1f" x)
            ; attr "x2" (sprintf "%.1f" x)
            ; attr "y1" (sprintf "%.1f" margin_top)
            ; attr "y2" (sprintf "%.1f" (margin_top +. plot_h))
            ]
          []
      ]
  in
  let svg_children =
    List.concat
      [ y_axis scale
      ; x_axis scale
      ; [ svg
            "path"
            ~attrs:
              [ Vdom.Attr.class_ "chart-vol"
              ; attr "d" (volume_path scale visible_bars)
              ]
            []
        ; svg
            "path"
            ~attrs:
              [ Vdom.Attr.class_ "chart-price"
              ; attr "d" (price_path scale visible_bars)
              ]
            []
        ]
      ; now_line
      ; (match hover with
         | None -> []
         | Some index -> crosshair scale day ~index)
      ; List.map fills ~f:(marker scale)
      ]
  in
  let seek_attr =
    (* Click-to-seek: any point on the chart jumps the playback there —
       including into the not-yet-drawn future, which is why the index is
       deliberately not clamped to [visible_upto]. *)
    match on_seek with
    | None -> Vdom.Attr.empty
    | Some seek ->
      Vdom.Attr.many
        [ Vdom.Attr.class_ "seekable"
        ; Vdom.Attr.on_click (fun event ->
            match hover_index_of_event event ~bar_count:scale.bar_count with
            | None -> Effect.Ignore
            | Some index -> seek index)
        ]
  in
  let chart_svg =
    svg
      "svg"
      ~attrs:
        [ Vdom.Attr.class_ "chart-svg"
        ; seek_attr
        ; attr "viewBox" (sprintf "0 0 %.0f %.0f" view_w view_h)
        ; Vdom.Attr.on_mousemove (fun event ->
            set_hover
              (Option.map
                 (hover_index_of_event event ~bar_count:scale.bar_count)
                 ~f:clamp_hover))
        ; Vdom.Attr.on_mouseleave (fun (_ : _ Js_of_ocaml.Js.t) ->
            set_hover None)
        ]
      svg_children
  in
  let tip =
    match hover with
    | None -> Vdom.Node.none
    | Some index -> tooltip scale day ~fills_by_minute ~index
  in
  let legend =
    if List.is_empty fills
    then Vdom.Node.none
    else (
      let swatch color =
        Vdom.Node.span
          ~attrs:
            [ Vdom.Attr.class_ "swatch"
            ; attr "style" ("background: " ^ color)
            ]
          []
      in
      Vdom.Node.div
        ~attrs:[ Vdom.Attr.class_ "legend" ]
        [ Vdom.Node.span [ swatch "var(--series-1)"; Vdom.Node.text "close" ]
        ; Vdom.Node.span
            [ swatch "var(--mk-buy)"; Vdom.Node.text "buy fills" ]
        ; Vdom.Node.span
            [ swatch "var(--mk-sell)"; Vdom.Node.text "sell fills" ]
        ])
  in
  {%html|
    <div>
      <div class="chart-wrap panel">
        %{chart_svg}
        %{tip}
      </div>
      %{legend}
    </div>
  |}
;;

let component
  ?(visible_upto = Bonsai.return None)
  ?(on_seek = Bonsai.return None)
  ~(day : Trading_day.t option Bonsai.t)
  ~(fills : Fill.t list Bonsai.t)
  (local_ graph)
  =
  let hover, set_hover =
    Bonsai.state ~equal:[%equal: int option] None graph
  in
  let%arr day
  and fills
  and visible_upto
  and on_seek
  and hover
  and set_hover in
  match day with
  | None -> empty_view
  | Some day -> render day fills ~visible_upto ~on_seek ~hover ~set_hover
;;
