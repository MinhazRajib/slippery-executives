open! Core
open! Bonsai_web
open Bonsai.Let_syntax
open! Execlab_types
open! Execlab_market
open! Execlab_simulation

module Screen = struct
  type t =
    | Setup
    | Sim
  [@@deriving sexp, equal]
end

let fs = sprintf "%.1f"

let dollars_signed cents =
  (if cents < 0 then "-$" else "+$")
  ^ sprintf "%.2f" (Float.abs (Float.of_int cents) /. 100.)
;;

(* ---------- shared bits ---------- *)

let transport_button ~active ~on_click label =
  let bg = if active then Styles.accent_soft else "transparent" in
  let color = if active then Styles.accent_bright else Styles.dim in
  let border =
    if active then "1px solid " ^ Styles.accent else Styles.border
  in
  let style =
    Styles.s
      ("background:"
       ^ bg
       ^ ";color:"
       ^ color
       ^ ";border:"
       ^ border
       ^ ";border-radius:2px;padding:3px \
          9px;cursor:pointer;font-size:10px;font-weight:700;letter-spacing:0.06em;text-transform:uppercase;"
      )
  in
  {%html|<button %{style} on_click=%{on_click}>#{label}</button>|}
;;

let header_row cols =
  let style =
    Styles.s
      ("display:flex;gap:12px;padding:5px 10px;border-bottom:"
       ^ Styles.border
       ^ ";")
  in
  let cell (width, label) =
    let sizing =
      if width < 0
      then "flex:1;text-align:right;"
      else sprintf "width:%dpx;" width
    in
    let s = Styles.s (Styles.micro ^ sizing) in
    {%html|<span %{s}>#{label}</span>|}
  in
  {%html|<div %{style}>*{List.map cols ~f:cell}</div>|}
;;

(* ---------- top bar ---------- *)

let top_bar
  ~clock
  ~progress_pct
  ~complete
  ~playing
  ~speed
  ~set_playing
  ~set_speed
  ~back
  =
  let bar =
    Styles.s
      ("display:flex;align-items:center;gap:12px;background:"
       ^ Styles.bg1
       ^ ";border:"
       ^ Styles.border
       ^ ";border-radius:2px;padding:7px 12px;")
  in
  let brand =
    Styles.s
      ("color:"
       ^ Styles.accent_bright
       ^ ";font-weight:800;letter-spacing:0.14em;font-size:12px;")
  in
  let chip =
    Styles.s
      (Styles.micro ^ "border-left:" ^ Styles.border ^ ";padding-left:12px;")
  in
  let clock_style =
    Styles.s
      ("color:"
       ^ Styles.text
       ^ ";font-size:16px;font-weight:700;"
       ^ Styles.mono)
  in
  let progress_outer =
    Styles.s
      ("flex:1;height:2px;background:" ^ Styles.bg2 ^ ";min-width:100px;")
  in
  let progress_inner =
    Styles.s
      (sprintf
         "height:100%%;width:%d%%;background:%s;"
         progress_pct
         Styles.accent)
  in
  let status =
    if complete
    then (
      let style = Styles.s (Styles.micro ^ "color:" ^ Styles.green ^ ";") in
      {%html|<span %{style}>■ complete</span>|})
    else if playing
    then (
      let style = Styles.s (Styles.micro ^ "color:" ^ Styles.orange ^ ";") in
      {%html|<span %{style}>▶ live</span>|})
    else (
      let style = Styles.s (Styles.micro ^ "color:" ^ Styles.faint ^ ";") in
      {%html|<span %{style}>II paused</span>|})
  in
  {%html|
    <div %{bar}>
      <span %{brand}>EXECLAB</span>
      <span %{chip}>TSLA · 2026-07-09 · 1m bars</span>
      <span %{clock_style}>#{clock}</span>
      <div %{progress_outer}><div %{progress_inner}></div></div>
      %{status}
      %{transport_button ~active:false
          ~on_click:(fun _ -> set_playing (not playing))
          (if playing then "pause" else "play")}
      %{transport_button ~active:(speed = 1) ~on_click:(fun _ -> set_speed 1) "1x"}
      %{transport_button ~active:(speed = 4) ~on_click:(fun _ -> set_speed 4) "4x"}
      %{transport_button ~active:(speed = 16) ~on_click:(fun _ -> set_speed 16) "16x"}
      %{transport_button ~active:false ~on_click:(fun _ -> back) "setup"}
    </div>
  |}
;;

(* ---------- stat strips: market vs execution ---------- *)

let stat ~first label value ~color =
  let cell_style =
    Styles.s
      ("padding:5px 12px;flex:1;"
       ^ if first then "" else "border-left:" ^ Styles.border ^ ";")
  in
  let label_style = Styles.s Styles.micro in
  let value_style =
    Styles.s
      ("color:" ^ color ^ ";font-size:13px;font-weight:700;" ^ Styles.mono)
  in
  {%html|
    <div %{cell_style}>
      <div %{label_style}>#{label}</div>
      <div %{value_style}>#{value}</div>
    </div>
  |}
;;

let strip ~title cells =
  let row = Styles.s "display:flex;align-items:stretch;" in
  {%html|
    <div %{Styles.panel ""}>
      <div %{Styles.panel_title}>#{title}</div>
      <div %{row}>*{cells}</div>
    </div>
  |}
;;

let market_strip (replay : Replay.t) ~minute =
  let bar_now = replay.bars.(minute) in
  let half_spread = Fill_model.Config.default.half_spread in
  let bid = Price.( - ) bar_now.open_ half_spread in
  let ask = Price.( + ) bar_now.open_ half_spread in
  let spread = Price.( - ) ask bid in
  let p = Price.to_string_dollar in
  strip
    ~title:"Market"
    [ stat ~first:true "last" (p bar_now.close) ~color:Styles.text
    ; stat ~first:false "bid" (p bid) ~color:Styles.dim
    ; stat ~first:false "ask" (p ask) ~color:Styles.dim
    ; stat ~first:false "spread" (p spread) ~color:Styles.dim
    ; stat
        ~first:false
        "session vwap"
        (sprintf "$%.2f" replay.vwap_by_minute.(minute))
        ~color:Styles.orange
    ; stat
        ~first:false
        "volume"
        (Int.to_string_hum ~delimiter:',' replay.cumulative_volume.(minute))
        ~color:Styles.dim
    ]
;;

let exec_strip (replay : Replay.t) ~fills ~minute =
  let bar_now = replay.bars.(minute) in
  let shares =
    List.sum (module Int) fills ~f:(fun (f : Fill.t) -> Size.to_int f.size)
  in
  let total =
    List.sum (module Int) replay.parents ~f:(fun parent ->
      Size.to_int parent.instruction.Alpha_instruction.quantity)
  in
  let notional =
    List.sum (module Int) fills ~f:(fun (f : Fill.t) ->
      Fill.notional_cents f)
  in
  let avg =
    if shares = 0 then "-" else sprintf "$%.2f" (notional // shares /. 100.)
  in
  let completion =
    if total = 0 then "-" else sprintf "%.1f%%" (shares // total *. 100.)
  in
  let pnl = Replay.open_pnl_cents ~fills ~last:bar_now.close in
  let pnl_color = if pnl >= 0 then Styles.green else Styles.red in
  let schedule, schedule_color =
    match Replay.schedule_delta_pct replay ~minute with
    | None -> "-", Styles.faint
    | Some delta ->
      let color =
        if Float.( < ) delta (-1.) then Styles.orange else Styles.green
      in
      sprintf "%+.1f%%" delta, color
  in
  strip
    ~title:"Execution"
    [ stat
        ~first:true
        "filled"
        (sprintf
           "%s / %s"
           (Int.to_string_hum ~delimiter:',' shares)
           (Int.to_string_hum ~delimiter:',' total))
        ~color:Styles.text
    ; stat ~first:false "complete" completion ~color:Styles.text
    ; stat ~first:false "avg fill" avg ~color:Styles.text
    ; stat ~first:false "open p&l" (dollars_signed pnl) ~color:pnl_color
    ; stat ~first:false "schedule" schedule ~color:schedule_color
    ; stat
        ~first:false
        "algo"
        (String.uppercase replay.algo_name)
        ~color:Styles.accent_bright
    ]
;;

(* ---------- price chart ---------- *)

let candle_chart (replay : Replay.t) ~slots ~stop ~fills =
  let bars = replay.bars in
  let n = Array.length bars in
  let w = 920. in
  let price_h = 320. in
  let vol_top = 338. in
  let vol_h = 62. in
  let h = vol_top +. vol_h in
  (* Fixed slot layout: candle width never changes during the run. The window
     is anchored left until it fills, then scrolls with the playhead. *)
  let offset = Int.max 0 (stop - slots + 1) in
  let layout_last = Int.min (n - 1) (offset + slots - 1) in
  let scale_range =
    Array.sub bars ~pos:offset ~len:(layout_last - offset + 1)
  in
  let lo =
    Array.fold scale_range ~init:Float.infinity ~f:(fun acc bar ->
      Float.min acc (Price.to_float bar.Market_bar.low))
  in
  let hi =
    Array.fold scale_range ~init:Float.neg_infinity ~f:(fun acc bar ->
      Float.max acc (Price.to_float bar.Market_bar.high))
  in
  let span = Float.max (hi -. lo) 0.01 in
  let max_vol =
    Array.fold scale_range ~init:1 ~f:(fun acc bar ->
      Int.max acc (Size.to_int bar.Market_bar.volume))
  in
  let cw = w /. Float.of_int slots in
  let x i = (Float.of_int (i - offset) *. cw) +. (cw /. 2.) in
  let y v = 6. +. ((hi -. v) /. span *. (price_h -. 12.)) in
  let svg name attrs children = Vdom.Node.create_svg name ~attrs children in
  let attr = Vdom.Attr.create in
  let tooltip text = svg "title" [] [ Vdom.Node.text text ] in
  let candle i =
    let bar = bars.(i) in
    let o = Price.to_float bar.Market_bar.open_ in
    let c = Price.to_float bar.Market_bar.close in
    let up = Float.( >= ) c o in
    let color = if up then Styles.green else Styles.red in
    let body_top = y (Float.max o c) in
    let body_h = Float.max 0.8 (Float.abs (y o -. y c)) in
    let title =
      tooltip
        (sprintf
           "%s  O %.2f  H %.2f  L %.2f  C %.2f  vol %s"
           (String.prefix (Time_ns.Ofday.to_string bar.Market_bar.time) 5)
           o
           (Price.to_float bar.Market_bar.high)
           (Price.to_float bar.Market_bar.low)
           c
           (Int.to_string_hum
              ~delimiter:','
              (Size.to_int bar.Market_bar.volume)))
    in
    let wick =
      svg
        "line"
        [ attr "x1" (fs (x i))
        ; attr "x2" (fs (x i))
        ; attr "y1" (fs (y (Price.to_float bar.Market_bar.high)))
        ; attr "y2" (fs (y (Price.to_float bar.Market_bar.low)))
        ; attr "stroke" color
        ; attr "stroke-width" (fs (Float.max 0.7 (cw *. 0.12)))
        ; attr "stroke-opacity" "0.7"
        ]
        []
    in
    let body =
      svg
        "rect"
        [ attr "x" (fs (x i -. (cw *. 0.32)))
        ; attr "y" (fs body_top)
        ; attr "width" (fs (cw *. 0.64))
        ; attr "height" (fs body_h)
        ; attr "fill" color
        ]
        []
    in
    let volume =
      let vh =
        Float.of_int (Size.to_int bar.Market_bar.volume)
        /. Float.of_int max_vol
        *. vol_h
      in
      svg
        "rect"
        [ attr "x" (fs (x i -. (cw *. 0.32)))
        ; attr "y" (fs (vol_top +. vol_h -. vh))
        ; attr "width" (fs (cw *. 0.64))
        ; attr "height" (fs vh)
        ; attr "fill" Styles.accent
        ; attr "fill-opacity" "0.35"
        ]
        []
    in
    svg "g" [] [ title; wick; body; volume ]
  in
  let drawn = List.init (stop - offset + 1) ~f:(fun k -> offset + k) in
  let candles = List.map drawn ~f:candle in
  let vwap_line =
    let pts =
      List.map drawn ~f:(fun i ->
        sprintf "%s,%s" (fs (x i)) (fs (y replay.vwap_by_minute.(i))))
      |> String.concat ~sep:" "
    in
    svg
      "polyline"
      [ attr "points" pts
      ; attr "fill" "none"
      ; attr "stroke" Styles.orange
      ; attr "stroke-width" "1"
      ; attr "stroke-opacity" "0.9"
      ]
      [ tooltip "session vwap" ]
  in
  let time_axis =
    let step =
      if slots > 240
      then 60
      else if slots > 120
      then 30
      else if slots > 60
      then 15
      else 5
    in
    List.filter_map
      (List.init (layout_last - offset + 1) ~f:(fun k -> offset + k))
      ~f:(fun i ->
        if i % step = 0 && i > 0
        then
          Some
            (svg
               "text"
               [ attr "x" (fs (x i))
               ; attr "y" (fs (vol_top -. 6.))
               ; attr "text-anchor" "middle"
               ; attr "fill" Styles.faint
               ; attr "font-size" "9"
               ]
               [ Vdom.Node.text
                   (String.prefix
                      (Time_ns.Ofday.to_string bars.(i).Market_bar.time)
                      5)
               ])
        else None)
  in
  let gridline frac =
    let gy = 6. +. (frac *. (price_h -. 12.)) in
    let price = hi -. (frac *. span) in
    [ svg
        "line"
        [ attr "x1" "0"
        ; attr "x2" (fs w)
        ; attr "y1" (fs gy)
        ; attr "y2" (fs gy)
        ; attr "stroke" Styles.hairline
        ; attr "stroke-width" "1"
        ]
        []
    ; svg
        "text"
        [ attr "x" (fs (w -. 4.))
        ; attr "y" (fs (gy -. 3.))
        ; attr "text-anchor" "end"
        ; attr "fill" Styles.faint
        ; attr "font-size" "9"
        ]
        [ Vdom.Node.text (sprintf "%.2f" price) ]
    ]
  in
  let grid = List.concat_map [ 0.; 0.25; 0.5; 0.75; 1. ] ~f:gridline in
  let last_close = Price.to_float bars.(stop).Market_bar.close in
  let now_line =
    [ svg
        "line"
        [ attr "x1" "0"
        ; attr "x2" (fs w)
        ; attr "y1" (fs (y last_close))
        ; attr "y2" (fs (y last_close))
        ; attr "stroke" Styles.accent_bright
        ; attr "stroke-width" "0.8"
        ; attr "stroke-dasharray" "3 3"
        ]
        []
    ; svg
        "text"
        [ attr "x" (fs 4.)
        ; attr "y" (fs (y last_close -. 4.))
        ; attr "fill" Styles.accent_bright
        ; attr "font-size" "10"
        ; attr "font-weight" "700"
        ]
        [ Vdom.Node.text (sprintf "%.2f" last_close) ]
    ]
  in
  let fill_markers =
    List.filter_map fills ~f:(fun (fill : Fill.t) ->
      let m = Replay.minute_of_time replay fill.time in
      if m < offset || m > stop
      then None
      else
        Some
          (svg
             "circle"
             [ attr "cx" (fs (x m))
             ; attr "cy" (fs (y (Price.to_float fill.price)))
             ; attr "r" "1.8"
             ; attr "fill" "#ffffff"
             ; attr "stroke" Styles.accent
             ; attr "stroke-width" "0.8"
             ]
             [ tooltip
                 (sprintf
                    "%s %s %d @ %s (%s)"
                    (String.prefix (Time_ns.Ofday.to_string fill.time) 5)
                    (match fill.side with
                     | Side.Buy -> "BUY"
                     | Sell -> "SELL")
                    (Size.to_int fill.size)
                    (Price.to_string_dollar fill.price)
                    (match fill.liquidity with
                     | Taker -> "taker"
                     | Maker -> "maker"))
             ]))
  in
  svg
    "svg"
    [ attr "viewBox" (sprintf "0 0 %s %s" (fs w) (fs h))
    ; attr "preserveAspectRatio" "none"
    ; Styles.s "width:100%;height:420px;display:block;"
    ]
    (grid @ time_axis @ candles @ (vwap_line :: now_line) @ fill_markers)
;;

(* ---------- tables and log ---------- *)

let parents_table (rows : Replay.parent_row list) =
  let row_view (row : Replay.parent_row) =
    let side, side_color =
      match row.side with
      | Side.Buy -> "BUY", Styles.text
      | Sell -> "SELL", Styles.text
    in
    let status_color =
      match row.status with
      | "DONE" -> Styles.green
      | "WORKING" -> Styles.orange
      | "EXPIRED" -> Styles.red
      | _ -> Styles.faint
    in
    let tr =
      Styles.s
        "display:flex;align-items:center;gap:12px;padding:6px \
         10px;border-bottom:1px solid rgba(255,255,255,0.04);"
    in
    let id_style = Styles.s (Styles.dim_cell ^ "width:30px;") in
    let side_style =
      Styles.s
        (sprintf
           "color:%s;font-weight:700;width:38px;%s"
           side_color
           Styles.mono)
    in
    let qty = Styles.s (Styles.cell ^ "width:110px;text-align:right;") in
    let window = Styles.s (Styles.dim_cell ^ "width:92px;") in
    let avg = Styles.s (Styles.cell ^ "width:64px;text-align:right;") in
    let status_style =
      Styles.s
        (Styles.micro ^ "color:" ^ status_color ^ ";flex:1;text-align:right;")
    in
    {%html|
      <div %{tr}>
        <span %{id_style}>#{row.id}</span>
        <span %{side_style}>#{side}</span>
        <span %{qty}>%{row.filled#Int} / %{row.total#Int}</span>
        <span %{window}>#{row.window}</span>
        <span %{avg}>#{row.avg_fill}</span>
        <span %{status_style}>#{row.status}</span>
      </div>
    |}
  in
  {%html|
    <div %{Styles.panel ""}>
      <div %{Styles.panel_title}>Parent orders</div>
      %{header_row
          [ 30, "id"; 38, "side"; 110, "filled"; 92, "window"; 64, "avg"
          ; -1, "status" ]}
      *{List.map rows ~f:row_view}
    </div>
  |}
;;

let event_log (events : Replay.event list) =
  let recent = List.rev events in
  let line_view (event : Replay.event) =
    let time = String.prefix (Time_ns.Ofday.to_string event.time) 5 in
    let is_fill = String.is_prefix event.line ~prefix:"FILL" in
    let color = if is_fill then Styles.dim else Styles.accent_bright in
    let tr =
      Styles.s
        ("display:flex;gap:10px;padding:2px 10px;font-size:12px;"
         ^ Styles.mono)
    in
    let t = Styles.s ("color:" ^ Styles.faint ^ ";") in
    let l = Styles.s ("color:" ^ color ^ ";") in
    {%html|<div %{tr}><span %{t}>#{time}</span><span %{l}>#{event.line}</span></div>|}
  in
  let body =
    Styles.s
      "padding:4px 0 8px \
       0;max-height:220px;overflow-y:auto;scrollbar-width:thin;"
  in
  {%html|
    <div %{Styles.panel ""}>
      <div %{Styles.panel_title}>Event log</div>
      <div %{body}>*{List.map recent ~f:line_view}</div>
    </div>
  |}
;;

let results_panel (replay : Replay.t) ~complete ~clock_close =
  if not complete
  then (
    let style = Styles.s (Styles.dim_cell ^ "padding:8px 10px;") in
    {%html|
      <div %{Styles.panel ""}>
        <div %{Styles.panel_title}>Results · vs immediate</div>
        <div %{style}>Available at close · #{clock_close}</div>
      </div>
    |})
  else (
    let results = replay.results in
    let row_view (row : Replay.result_row) =
      let side, side_color =
        match row.side with
        | Side.Buy -> "BUY", Styles.green
        | Sell -> "SELL", Styles.red
      in
      let va_color =
        if row.value_add_cents >= 0 then Styles.green else Styles.red
      in
      let tr =
        Styles.s
          "display:flex;align-items:center;gap:12px;padding:5px \
           10px;border-bottom:1px solid rgba(255,255,255,0.04);"
      in
      let side_style =
        Styles.s
          (sprintf
             "color:%s;font-weight:700;width:38px;%s"
             side_color
             Styles.mono)
      in
      let qty = Styles.s (Styles.cell ^ "width:40px;text-align:right;") in
      let avg = Styles.s (Styles.cell ^ "width:60px;text-align:right;") in
      let bps =
        Styles.s (Styles.dim_cell ^ "width:48px;text-align:right;")
      in
      let va =
        Styles.s
          (sprintf
             "color:%s;font-weight:700;flex:1;text-align:right;%sfont-size:12px;"
             va_color
             Styles.mono)
      in
      {%html|
        <div %{tr}>
          <span %{side_style}>#{side}</span>
          <span %{qty}>%{row.quantity#Int}</span>
          <span %{avg}>#{row.avg_fill}</span>
          <span %{bps}>#{row.shortfall_bps}bp</span>
          <span %{va}>#{dollars_signed row.value_add_cents}</span>
        </div>
      |}
    in
    let total = results.total_value_add_cents in
    let total_color = if total >= 0 then Styles.green else Styles.red in
    let footer =
      Styles.s "display:flex;align-items:baseline;gap:10px;padding:8px 10px;"
    in
    let footer_label = Styles.s Styles.micro in
    let footer_value =
      Styles.s
        (sprintf
           "color:%s;font-size:18px;font-weight:800;%s"
           total_color
           Styles.mono)
    in
    {%html|
      <div %{Styles.panel ""}>
        <div %{Styles.panel_title}>Results · vs immediate</div>
        %{header_row
            [ 38, "side"; 40, "qty"; 60, "avg"; 48, "bps"; -1, "value add" ]}
        *{List.map results.rows ~f:row_view}
        <div %{footer}>
          <span %{footer_label}>Execution value add</span>
          <span %{footer_value}>#{dollars_signed total}</span>
        </div>
      </div>
    |})
;;

(* ---------- screens ---------- *)

let sim_view
  (replay : Replay.t)
  ~minute
  ~playing
  ~speed
  ~zoom
  ~set_playing
  ~set_speed
  ~set_zoom
  ~back
  =
  let last = Replay.last_minute replay in
  let fills = Replay.fills_upto replay ~minute in
  let events = Replay.events_upto replay ~minute in
  let rows = Replay.parent_rows replay ~minute in
  let complete = minute >= last in
  let slots =
    match zoom with
    | None -> Array.length replay.bars
    | Some window -> window
  in
  let zoom_button ~window label =
    transport_button
      ~active:
        (match zoom, window with
         | None, None -> true
         | Some a, Some b -> a = b
         | None, Some _ | Some _, None -> false)
      ~on_click:(fun _ -> set_zoom window)
      label
  in
  let page =
    Styles.s
      "display:flex;flex-direction:column;gap:10px;max-width:1600px;margin:0 \
       auto;padding:12px;"
  in
  let strips =
    Styles.s "display:grid;grid-template-columns:1fr 1fr;gap:10px;"
  in
  let grid =
    Styles.s
      "display:grid;grid-template-columns:minmax(0,1fr) \
       340px;gap:10px;align-items:start;"
  in
  let col = Styles.s "display:flex;flex-direction:column;gap:10px;" in
  let chart_header =
    Styles.s
      ("display:flex;align-items:center;gap:8px;padding:6px \
        10px;border-bottom:"
       ^ Styles.border
       ^ ";")
  in
  let chart_title = Styles.s (Styles.micro ^ "flex:1;") in
  {%html|
    <div %{page}>
      %{top_bar ~clock:(Replay.clock_string replay ~minute)
          ~progress_pct:(minute * 100 / last) ~complete ~playing ~speed
          ~set_playing ~set_speed ~back}
      <div %{strips}>
        %{market_strip replay ~minute}
        %{exec_strip replay ~fills ~minute}
      </div>
      <div %{grid}>
        <div %{col}>
          <div %{Styles.panel ""}>
            <div %{chart_header}>
              <span %{chart_title}>TSLA · candles · vwap · fills</span>
              %{zoom_button ~window:(Some 30) "30m"}
              %{zoom_button ~window:(Some 60) "1h"}
              %{zoom_button ~window:(Some 120) "2h"}
              %{zoom_button ~window:None "full"}
            </div>
            %{candle_chart replay ~slots ~stop:minute ~fills}
          </div>
        </div>
        <div %{col}>
          %{parents_table rows}
          %{results_panel replay ~complete
              ~clock_close:(Replay.clock_string replay ~minute:last)}
        </div>
      </div>
      %{event_log events}
    </div>
  |}
;;

let algo_card ~selected ~on_click ~name ~blurb =
  let border =
    if selected then "1px solid " ^ Styles.accent else Styles.border
  in
  let bg = if selected then Styles.accent_soft else Styles.bg1 in
  let style =
    Styles.s
      ("background:"
       ^ bg
       ^ ";border:"
       ^ border
       ^ ";border-radius:2px;padding:12px 14px;cursor:pointer;flex:1;")
  in
  let name_style =
    Styles.s
      ("color:"
       ^ Styles.text
       ^ ";font-weight:800;font-size:13px;letter-spacing:0.04em;")
  in
  let blurb_style =
    Styles.s
      ("color:"
       ^ Styles.dim
       ^ ";font-size:12px;margin-top:5px;line-height:1.5;")
  in
  {%html|
    <div %{style} on_click=%{on_click}>
      <div %{name_style}>#{name}</div>
      <div %{blurb_style}>#{blurb}</div>
    </div>
  |}
;;

let setup_view ~algo ~set_algo ~start =
  let page =
    Styles.s
      "display:flex;flex-direction:column;gap:12px;max-width:720px;margin:48px \
       auto;padding:12px;"
  in
  let brand =
    Styles.s
      ("color:"
       ^ Styles.accent_bright
       ^ ";font-weight:800;letter-spacing:0.14em;font-size:12px;")
  in
  let title =
    Styles.s
      ("color:"
       ^ Styles.text
       ^ ";font-size:20px;font-weight:800;margin-top:4px;")
  in
  let subtitle = Styles.s (Styles.micro ^ "margin-top:4px;") in
  let cards = Styles.s "display:flex;gap:10px;" in
  let start_style =
    Styles.s
      ("background:"
       ^ Styles.accent
       ^ ";color:#ffffff;border:none;border-radius:2px;padding:10px \
          20px;font-size:11px;font-weight:800;letter-spacing:0.1em;text-transform:uppercase;cursor:pointer;align-self:flex-start;"
      )
  in
  let instruction_row (instruction : Alpha_instruction.t) =
    let side, side_color =
      match instruction.side with
      | Side.Buy -> "BUY", Styles.text
      | Sell -> "SELL", Styles.text
    in
    let arrival =
      String.prefix (Time_ns.Ofday.to_string instruction.arrival_time) 5
    in
    let deadline =
      String.prefix (Time_ns.Ofday.to_string instruction.deadline) 5
    in
    let tr =
      Styles.s
        "display:flex;gap:12px;padding:5px 10px;border-bottom:1px solid \
         rgba(255,255,255,0.04);"
    in
    let side_style =
      Styles.s
        (sprintf
           "color:%s;font-weight:700;width:38px;%s"
           side_color
           Styles.mono)
    in
    let qty = Styles.s (Styles.cell ^ "width:70px;text-align:right;") in
    let window = Styles.s (Styles.dim_cell ^ "flex:1;") in
    {%html|
      <div %{tr}>
        <span %{side_style}>#{side}</span>
        <span %{qty}>%{Size.to_int instruction.quantity#Int}</span>
        <span %{window}>#{arrival} → #{deadline}</span>
      </div>
    |}
  in
  {%html|
    <div %{page}>
      <div>
        <div %{brand}>EXECLAB</div>
        <div %{title}>New simulation</div>
        <div %{subtitle}>TSLA · 2026-07-09 · bar-based fill model</div>
      </div>
      <div %{cards}>
        %{algo_card ~selected:(String.equal algo "twap")
            ~on_click:(fun _ -> set_algo "twap") ~name:"TWAP"
            ~blurb:"Slices the order evenly across the instruction window; \
                    always on schedule, catches up automatically."}
        %{algo_card ~selected:(String.equal algo "immediate")
            ~on_click:(fun _ -> set_algo "immediate") ~name:"IMMEDIATE"
            ~blurb:"The naive baseline: the full order as one market order \
                    the moment the instruction arrives."}
      </div>
      <div %{Styles.panel ""}>
        <div %{Styles.panel_title}>Alpha instructions</div>
        %{header_row [ 38, "side"; 70, "qty"; 120, "window" ]}
        *{List.map (Replay.demo_instructions ()) ~f:instruction_row}
      </div>
      <button %{start_style} on_click=%{fun _ -> start}>
        Run simulation
      </button>
    </div>
  |}
;;

(* ---------- the app ---------- *)

let app (local_ graph) : Vdom.Node.t Bonsai.t =
  let screen, set_screen = Bonsai.state Screen.Setup graph in
  let algo, set_algo = Bonsai.state "twap" graph in
  let replay, set_replay = Bonsai.state (None : Replay.t option) graph in
  let minute, set_minute = Bonsai.state' 0 graph in
  let playing, set_playing = Bonsai.state true graph in
  let speed, set_speed = Bonsai.state 4 graph in
  let zoom, set_zoom = Bonsai.state (None : int option) graph in
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
    and set_replay
    and set_screen
    and set_minute
    and set_playing in
    let%bind.Effect r =
      Effect.of_sync_fun (fun () -> Replay.run ~algo_name:algo) ()
    in
    let%bind.Effect () = set_replay (Some r) in
    let%bind.Effect () = set_minute (fun _ -> 0) in
    let%bind.Effect () = set_playing true in
    set_screen Screen.Sim
  in
  let%arr screen
  and algo
  and set_algo
  and replay
  and minute
  and playing
  and set_playing
  and speed
  and set_speed
  and zoom
  and set_zoom
  and set_screen
  and start in
  let body =
    match screen, replay with
    | Screen.Sim, Some r ->
      sim_view
        r
        ~minute
        ~playing
        ~speed
        ~zoom
        ~set_playing
        ~set_speed
        ~set_zoom
        ~back:(set_screen Screen.Setup)
    | Setup, _ | Sim, None -> setup_view ~algo ~set_algo ~start
  in
  let shell =
    Styles.s
      ("min-height:100vh;background:"
       ^ Styles.bg0
       ^ ";color-scheme:dark;font-family:system-ui,sans-serif;")
  in
  {%html|<div %{shell}>%{body}</div>|}
;;

let () = Bonsai_web.Start.start app
