open! Core
open! Bonsai_web
open Bonsai.Let_syntax
open! Execlab_types
open! Execlab_market

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

(* ---------- top bar ---------- *)

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
          9px;cursor:pointer;font-size:10px;font-weight:700;letter-spacing:0.08em;text-transform:uppercase;"
      )
  in
  {%html|<button %{style} on_click=%{on_click}>#{label}</button>|}
;;

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
       ^ ";font-weight:800;letter-spacing:0.18em;font-size:12px;")
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
      let style =
        Styles.s
          ("color:"
           ^ Styles.green
           ^ ";"
           ^ Styles.micro
           ^ "color:"
           ^ Styles.green
           ^ ";")
      in
      {%html|<span %{style}>&#9632; complete</span>|})
    else if playing
    then (
      let style =
        Styles.s
          ("color:"
           ^ Styles.orange
           ^ ";"
           ^ Styles.micro
           ^ "color:"
           ^ Styles.orange
           ^ ";")
      in
      {%html|<span %{style}>&#9654; live</span>|})
    else (
      let style = Styles.s (Styles.micro ^ "color:" ^ Styles.faint ^ ";") in
      {%html|<span %{style}>&#10073;&#10073; paused</span>|})
  in
  {%html|
    <div %{bar}>
      <span %{brand}>EXECLAB</span>
      <span %{chip}>TSLA &middot; 2026-07-09 &middot; 1m bars</span>
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

(* ---------- stat strip ---------- *)

let stat_strip (replay : Replay.t) ~fills ~minute =
  let bar_now = replay.bars.(minute) in
  let sofar f =
    Array.foldi replay.bars ~init:None ~f:(fun i acc bar ->
      if i > minute then acc else f acc bar)
  in
  let high_sofar =
    sofar (fun acc bar ->
      let v = bar.Market_bar.high in
      match acc with None -> Some v | Some a -> Some (Price.max a v))
    |> Option.value_exn
  in
  let low_sofar =
    sofar (fun acc bar ->
      let v = bar.Market_bar.low in
      match acc with None -> Some v | Some a -> Some (Price.min a v))
    |> Option.value_exn
  in
  let shares =
    List.sum (module Int) fills ~f:(fun (f : Fill.t) -> Size.to_int f.size)
  in
  let notional =
    List.sum (module Int) fills ~f:(fun (f : Fill.t) ->
      Fill.notional_cents f)
  in
  let avg =
    if shares = 0 then "-" else sprintf "$%.2f" (notional // shares /. 100.)
  in
  let stat ~first label value ~color =
    let cell_style =
      Styles.s
        ("padding:6px 14px;"
         ^ if first then "" else "border-left:" ^ Styles.border ^ ";")
    in
    let label_style = Styles.s Styles.micro in
    let value_style =
      Styles.s
        ("color:" ^ color ^ ";font-size:14px;font-weight:700;" ^ Styles.mono)
    in
    {%html|
      <div %{cell_style}>
        <div %{label_style}>#{label}</div>
        <div %{value_style}>#{value}</div>
      </div>
    |}
  in
  let pnl = Replay.open_pnl_cents ~fills ~last:bar_now.close in
  let pnl_color = if pnl >= 0 then Styles.green else Styles.red in
  let p = Price.to_string_dollar in
  let row = Styles.s "display:flex;align-items:stretch;" in
  {%html|
    <div %{Styles.panel ""}>
      <div %{row}>
        %{stat ~first:true "last" (p bar_now.close) ~color:Styles.text}
        %{stat ~first:false "open p&l" (dollars_signed pnl) ~color:pnl_color}
        %{stat ~first:false "high" (p high_sofar) ~color:Styles.green}
        %{stat ~first:false "low" (p low_sofar) ~color:Styles.red}
        %{stat ~first:false "bar vol" (Int.to_string_hum ~delimiter:','
            (Size.to_int bar_now.volume)) ~color:Styles.dim}
        %{stat ~first:false "filled" (Int.to_string_hum ~delimiter:',' shares)
            ~color:Styles.text}
        %{stat ~first:false "avg fill" avg ~color:Styles.text}
        %{stat ~first:false "algo" (String.uppercase replay.algo_name)
            ~color:Styles.accent_bright}
      </div>
    </div>
  |}
;;

(* ---------- candlestick chart with volume band and fill markers ---------- *)

let minute_of_time (replay : Replay.t) time =
  let open_time = replay.bars.(0).Market_bar.time in
  Float.to_int (Time_ns.Span.to_min (Time_ns.Ofday.diff time open_time))
;;

let candle_chart (replay : Replay.t) ~minute ~fills =
  let bars = replay.bars in
  let n = Array.length bars in
  let w = 920. in
  let price_h = 196. in
  let vol_top = 208. in
  let vol_h = 44. in
  let h = vol_top +. vol_h in
  let lo =
    Array.fold bars ~init:Float.infinity ~f:(fun acc bar ->
      Float.min acc (Price.to_float bar.Market_bar.low))
  in
  let hi =
    Array.fold bars ~init:Float.neg_infinity ~f:(fun acc bar ->
      Float.max acc (Price.to_float bar.Market_bar.high))
  in
  let max_vol =
    Array.fold bars ~init:1 ~f:(fun acc bar ->
      Int.max acc (Size.to_int bar.Market_bar.volume))
  in
  let cw = w /. Float.of_int n in
  let x i = (Float.of_int i *. cw) +. (cw /. 2.) in
  let y v = 4. +. ((hi -. v) /. (hi -. lo) *. (price_h -. 8.)) in
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
        ; attr "stroke-width" "0.7"
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
  let candles = List.map (List.init (minute + 1) ~f:Fn.id) ~f:candle in
  let vwap_line =
    let pts =
      List.init (minute + 1) ~f:(fun i ->
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
    List.filter_map (List.init n ~f:Fn.id) ~f:(fun i ->
      if i % 60 = 0 && i > 0
      then
        Some
          (svg
             "text"
             [ attr "x" (fs (x i))
             ; attr "y" (fs (vol_top -. 4.))
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
    let gy = 4. +. (frac *. (price_h -. 8.)) in
    let price = hi -. (frac *. (hi -. lo)) in
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
  let last_close = Price.to_float bars.(minute).Market_bar.close in
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
    List.map fills ~f:(fun (fill : Fill.t) ->
      let m = minute_of_time replay fill.time in
      let side_str =
        match fill.side with Side.Buy -> "BUY" | Sell -> "SELL"
      in
      svg
        "circle"
        [ attr "cx" (fs (x m))
        ; attr "cy" (fs (y (Price.to_float fill.price)))
        ; attr "r" "1.3"
        ; attr "fill" "#ffffff"
        ; attr "stroke" Styles.accent
        ; attr "stroke-width" "0.8"
        ]
        [ tooltip
            (sprintf
               "%s %s %d @ %s (%s)"
               (String.prefix (Time_ns.Ofday.to_string fill.time) 5)
               side_str
               (Size.to_int fill.size)
               (Price.to_string_dollar fill.price)
               (match fill.liquidity with
                | Taker -> "taker"
                | Maker -> "maker"))
        ])
  in
  svg
    "svg"
    [ attr "viewBox" (sprintf "0 0 %s %s" (fs w) (fs h))
    ; attr "preserveAspectRatio" "none"
    ; Styles.s "width:100%;height:280px;display:block;"
    ]
    (grid @ time_axis @ candles @ (vwap_line :: now_line) @ fill_markers)
;;

(* ---------- tables ---------- *)

let header_row cols =
  let style =
    Styles.s
      ("display:flex;gap:12px;padding:5px 10px;border-bottom:"
       ^ Styles.border
       ^ ";")
  in
  let cell (width, label) =
    let s = Styles.s (Styles.micro ^ sprintf "width:%dpx;" width) in
    {%html|<span %{s}>#{label}</span>|}
  in
  {%html|<div %{style}>*{List.map cols ~f:cell}</div>|}
;;

let parents_table rows =
  let row_view (row : Replay.parent_row) =
    let side, side_color =
      match row.side with
      | Side.Buy -> "BUY", Styles.green
      | Sell -> "SELL", Styles.red
    in
    let status_color =
      match row.status with
      | "DONE" -> Styles.green
      | "WORKING" -> Styles.orange
      | "EXPIRED" -> Styles.red
      | _ -> Styles.faint
    in
    let pct = if row.total = 0 then 0 else row.filled * 100 / row.total in
    let tr =
      Styles.s
        "display:flex;align-items:center;gap:12px;padding:6px \
         10px;border-bottom:1px solid rgba(255,255,255,0.04);"
    in
    let side_style =
      Styles.s
        (sprintf
           "color:%s;font-weight:700;width:44px;%s"
           side_color
           Styles.mono)
    in
    let num = Styles.s (Styles.cell ^ "width:110px;text-align:right;") in
    let bar_outer =
      Styles.s ("width:110px;height:3px;background:" ^ Styles.bg2 ^ ";")
    in
    let bar_inner =
      Styles.s
        (sprintf "height:100%%;width:%d%%;background:%s;" pct Styles.accent)
    in
    let status_style =
      Styles.s (Styles.micro ^ "color:" ^ status_color ^ ";width:70px;")
    in
    {%html|
      <div %{tr}>
        <span %{side_style}>#{side}</span>
        <span %{num}>%{row.filled#Int} / %{row.total#Int}</span>
        <div %{bar_outer}><div %{bar_inner}></div></div>
        <span %{status_style}>#{row.status}</span>
      </div>
    |}
  in
  {%html|
    <div %{Styles.panel ""}>
      <div %{Styles.panel_title}>Parent orders</div>
      *{List.map rows ~f:row_view}
    </div>
  |}
;;

let fills_table (fills : Fill.t list) =
  let recent = List.rev fills in
  let row_view (fill : Fill.t) =
    let side, side_color =
      match fill.side with
      | Side.Buy -> "B", Styles.green
      | Sell -> "S", Styles.red
    in
    let liq_color =
      match fill.liquidity with
      | Liquidity.Taker -> Styles.orange
      | Maker -> Styles.green
    in
    let time = String.prefix (Time_ns.Ofday.to_string fill.time) 5 in
    let tr =
      Styles.s
        "display:flex;align-items:center;gap:12px;padding:4px \
         10px;border-bottom:1px solid rgba(255,255,255,0.04);"
    in
    let time_style = Styles.s (Styles.dim_cell ^ "width:44px;") in
    let side_style =
      Styles.s
        (sprintf
           "color:%s;font-weight:700;width:14px;%s"
           side_color
           Styles.mono)
    in
    let qty = Styles.s (Styles.cell ^ "width:46px;text-align:right;") in
    let price = Styles.s (Styles.cell ^ "width:70px;text-align:right;") in
    {%html|
      <div %{tr}>
        <span %{time_style}>#{time}</span>
        <span %{side_style}>#{side}</span>
        <span %{qty}>%{Size.to_int fill.size#Int}</span>
        <span %{price}>#{Price.to_string_dollar fill.price}</span>
        <span %{Styles.dot liq_color}></span>
      </div>
    |}
  in
  let empty =
    let style = Styles.s (Styles.dim_cell ^ "padding:8px 10px;") in
    {%html|<div %{style}>Waiting for fills...</div>|}
  in
  let scroll =
    Styles.s "max-height:300px;overflow-y:auto;scrollbar-width:thin;"
  in
  {%html|
    <div %{Styles.panel ""}>
      <div %{Styles.panel_title}>Recent fills</div>
      %{header_row [ 44, "time"; 14, "s"; 46, "qty"; 70, "price"; 20, "liq" ]}
      %{if List.is_empty recent then empty else Vdom.Node.none}
      <div %{scroll}>*{List.map recent ~f:row_view}</div>
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
        ("display:flex;gap:10px;padding:2px 10px;font-size:11px;"
         ^ Styles.mono)
    in
    let t = Styles.s ("color:" ^ Styles.faint ^ ";") in
    let l = Styles.s ("color:" ^ color ^ ";") in
    {%html|<div %{tr}><span %{t}>#{time}</span><span %{l}>#{event.line}</span></div>|}
  in
  let body =
    Styles.s
      "padding:4px 0 8px \
       0;max-height:180px;overflow-y:auto;scrollbar-width:thin;"
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
        <div %{Styles.panel_title}>Results &middot; vs immediate</div>
        <div %{style}>Available at close &middot; #{clock_close}</div>
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
      let qty = Styles.s (Styles.cell ^ "width:44px;text-align:right;") in
      let avg = Styles.s (Styles.cell ^ "width:64px;text-align:right;") in
      let bps =
        Styles.s (Styles.dim_cell ^ "width:56px;text-align:right;")
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
        <div %{Styles.panel_title}>Results &middot; vs immediate</div>
        %{header_row
            [ 38, "side"; 44, "qty"; 64, "avg"; 56, "shortfall"; 60, "value add" ]}
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
  ~set_playing
  ~set_speed
  ~back
  =
  let last = Replay.last_minute replay in
  let fills = Replay.fills_upto replay ~minute in
  let events = Replay.events_upto replay ~minute in
  let rows = Replay.parent_rows replay ~minute in
  let complete = minute >= last in
  let page =
    Styles.s
      "display:flex;flex-direction:column;gap:10px;max-width:1320px;margin:0 \
       auto;padding:12px;"
  in
  let grid =
    Styles.s
      "display:grid;grid-template-columns:minmax(0,1fr) \
       330px;gap:10px;align-items:start;"
  in
  let col = Styles.s "display:flex;flex-direction:column;gap:10px;" in
  {%html|
    <div %{page}>
      %{top_bar ~clock:(Replay.clock_string replay ~minute)
          ~progress_pct:(minute * 100 / last) ~complete ~playing ~speed
          ~set_playing ~set_speed ~back}
      %{stat_strip replay ~fills ~minute}
      <div %{grid}>
        <div %{col}>
          <div %{Styles.panel ""}>
            <div %{Styles.panel_title}>TSLA &middot; candles + volume &middot; fills marked</div>
            %{candle_chart replay ~minute ~fills}
          </div>
          %{parents_table rows}
          %{event_log events}
        </div>
        <div %{col}>
          %{fills_table fills}
          %{results_panel replay ~complete
              ~clock_close:(Replay.clock_string replay ~minute:last)}
        </div>
      </div>
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
       ^ ";font-weight:800;font-size:13px;letter-spacing:0.06em;")
  in
  let blurb_style =
    Styles.s
      ("color:"
       ^ Styles.dim
       ^ ";font-size:11px;margin-top:5px;line-height:1.5;")
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
       ^ ";font-weight:800;letter-spacing:0.18em;font-size:12px;")
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
          20px;font-size:11px;font-weight:800;letter-spacing:0.12em;text-transform:uppercase;cursor:pointer;align-self:flex-start;"
      )
  in
  let instruction_row (instruction : Alpha_instruction.t) =
    let side, side_color =
      match instruction.side with
      | Side.Buy -> "BUY", Styles.green
      | Sell -> "SELL", Styles.red
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
        <span %{window}>#{arrival} &rarr; #{deadline}</span>
      </div>
    |}
  in
  {%html|
    <div %{page}>
      <div>
        <div %{brand}>EXECLAB</div>
        <div %{title}>New simulation</div>
        <div %{subtitle}>TSLA &middot; 2026-07-09 &middot; bar-based fill model</div>
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
        ~set_playing
        ~set_speed
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
