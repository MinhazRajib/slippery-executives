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

let side_str side =
  match (side : Side.t) with Buy -> "BUY" | Sell -> "SELL"
;;

(* Side-adjusted bps vs a benchmark: positive means worse (paid more /
   received less), so red is always bad and green always good. *)
let bps_vs ~(side : Side.t) ~avg ~benchmark =
  Float.of_int (Side.sign side) *. (avg -. benchmark) /. benchmark *. 10000.
;;

let bps_view value =
  let color = if Float.( <= ) value 0. then Styles.green else Styles.red in
  let style =
    Styles.s
      ("color:" ^ color ^ ";font-size:14px;font-weight:600;" ^ Styles.mono)
  in
  let unit_style = Styles.s ("color:" ^ Styles.faint ^ ";font-size:11px;") in
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
let money_stat ~label:text value_cents =
  let color =
    if value_cents > 0
    then Styles.green
    else if value_cents < 0
    then Styles.red
    else Styles.secondary
  in
  let label_style =
    Styles.s ("color:" ^ Styles.faint ^ ";font-size:12px;")
  in
  let value_style =
    Styles.s
      ("color:" ^ color ^ ";font-size:13px;font-weight:600;" ^ Styles.mono)
  in
  {%html|
    <span>
      <span %{label_style}>#{text}</span>
      <span %{value_style}>#{dollars_signed value_cents}</span>
    </span>
  |}
;;

(* ---------- controls ---------- *)

let pill ~active ~on_click label =
  let bg = if active then Styles.text else "transparent" in
  let color = if active then "#ffffff" else Styles.secondary in
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
       ^ Styles.blue
       ^ ";color:#ffffff;border:none;border-radius:5px;padding:8px \
          16px;cursor:pointer;font-size:13px;font-weight:700;white-space:nowrap;"
      )
  in
  let group =
    Styles.s
      "display:flex;gap:2px;background:#eef1f6;border-radius:5px;padding:2px;"
  in
  let slider_style =
    Styles.s ("flex:1;accent-color:" ^ Styles.blue ^ ";min-width:160px;")
  in
  let clock_style =
    Styles.s
      ("color:"
       ^ Styles.text
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
    Styles.s ("color:" ^ Styles.faint ^ ";font-size:13px;white-space:nowrap;")
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
    <div %{Styles.card ""}>
      <div %{row}>
        <button %{primary} on_click=%{fun _ -> restart}>Replay day</button>
        <div %{group}>
          %{pill
              ~active:(playing && not complete)
              ~on_click:(fun _ -> set_playing (not playing))
              (if playing && not complete then "Pause" else "Play")}
          %{pill ~active:(speed = 1) ~on_click:(fun _ -> set_speed 1) "1x"}
          %{pill ~active:(speed = 4) ~on_click:(fun _ -> set_speed 4) "4x"}
          %{pill ~active:(speed = 16) ~on_click:(fun _ -> set_speed 16) "16x"}
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

(* ---------- the chart: price line, order windows, fill tick rows ---------- *)

let chart (replay : Replay.t) ~minute ~fills ~show_fills =
  let bars = replay.bars in
  let n = Array.length bars in
  let n_orders = List.length replay.parents in
  let w = 1140. in
  let left = 52. in
  let right = 70. in
  let plot_w = w -. left -. right in
  let top = 10. in
  let price_h = 290. in
  let axis_y = top +. price_h +. 18. in
  let ticks_top = axis_y +. 12. in
  let tick_row_h = 17. in
  let h = ticks_top +. (Float.of_int n_orders *. tick_row_h) +. 8. in
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
          ; attr "stroke" Styles.hairline
          ; attr "stroke-width" "1"
          ]
          []
      ; svg
          "text"
          [ attr "x" (fs (left -. 8.))
          ; attr "y" (fs (y v +. 3.))
          ; attr "text-anchor" "end"
          ; attr "fill" Styles.faint
          ; attr "font-size" "11"
          ]
          [ Vdom.Node.text (sprintf "$%.0f" v) ]
      ])
  in
  (* shaded execution window + dashed arrival-price annotation per order *)
  let windows =
    List.concat_mapi replay.parents ~f:(fun index parent ->
      let color = Styles.order_color index in
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
          ; attr "fill-opacity" "0.07"
          ]
          [ tooltip
              (sprintf
                 "order %d window %s-%s"
                 (index + 1)
                 (hhmm parent.instruction.Alpha_instruction.arrival_time)
                 (hhmm parent.instruction.Alpha_instruction.deadline))
          ]
      ; svg
          "line"
          [ attr "x1" (fs x0)
          ; attr "x2" (fs x1)
          ; attr "y1" (fs (y arrival))
          ; attr "y2" (fs (y arrival))
          ; attr "stroke" Styles.faint
          ; attr "stroke-width" "1"
          ; attr "stroke-dasharray" "4 3"
          ]
          []
      ; svg
          "text"
          [ attr "x" (fs (x0 +. 4.))
          ; attr "y" (fs (y arrival -. 5.))
          ; attr "fill" Styles.faint
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
        ; attr "stroke" Styles.orange
        ; attr "stroke-width" "1.5"
        ; attr "stroke-dasharray" "5 4"
        ]
        []
    ; svg
        "text"
        [ attr "x" (fs (left +. plot_w +. 6.))
        ; attr "y" (fs (y day_vwap +. 3.))
        ; attr "fill" Styles.orange
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
        ; attr "stroke" Styles.blue
        ; attr "stroke-width" "1.8"
        ; attr "stroke-linejoin" "round"
        ]
        []
    ; svg
        "circle"
        [ attr "cx" (fs (x minute))
        ; attr "cy" (fs (y (Price.to_float bars.(minute).Market_bar.close)))
        ; attr "r" "4"
        ; attr "fill" Styles.blue
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
             ; attr "fill" Styles.faint
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
          ; attr "fill" (Styles.order_color index)
          ; attr "stroke" "#ffffff"
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
  (* one strip of fill ticks per order, below the time axis *)
  let tick_rows =
    List.concat_mapi replay.parents ~f:(fun index parent ->
      let color = Styles.order_color index in
      let row_y = ticks_top +. (Float.of_int index *. tick_row_h) in
      let label =
        svg
          "text"
          [ attr "x" "2"
          ; attr "y" (fs (row_y +. 9.))
          ; attr "text-anchor" "start"
          ; attr "fill" Styles.faint
          ; attr "font-size" "10"
          ; attr "font-weight" "700"
          ]
          [ Vdom.Node.text
              (sprintf
                 "O%d %s"
                 (index + 1)
                 (side_str parent.instruction.Alpha_instruction.side))
          ]
      in
      let ticks =
        List.filter_map fills ~f:(fun (fill : Fill.t) ->
          if not (Set.mem parent.order_ids fill.order_id)
          then None
          else (
            let m = Replay.minute_of_time replay fill.time in
            Some
              (svg
                 "rect"
                 [ attr "x" (fs (x m -. 1.))
                 ; attr "y" (fs row_y)
                 ; attr "width" "2.4"
                 ; attr "height" "11"
                 ; attr "fill" color
                 ]
                 [ tooltip
                     (sprintf
                        "%s %s %d @ %s (%s)"
                        (hhmm fill.time)
                        (side_str fill.side)
                        (Size.to_int fill.size)
                        (Price.to_string_dollar fill.price)
                        (match fill.liquidity with
                         | Taker -> "taker"
                         | Maker -> "maker"))
                 ])))
      in
      label :: ticks)
  in
  svg
    "svg"
    [ attr "viewBox" (sprintf "0 0 %s %s" (fs w) (fs h))
    ; Styles.s "width:100%;display:block;"
    ]
    (grid
     @ windows
     @ vwap_line
     @ time_axis
     @ price_line
     @ fill_dots
     @ tick_rows)
;;

let legend (replay : Replay.t) ~minute ~fills ~show_fills ~toggle_fills =
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
      Styles.s ("color:" ^ Styles.secondary ^ ";font-size:13px;")
    in
    {%html|<span><span %{swatch}></span> <span %{text_style}>#{label}</span></span>|}
  in
  let order_items =
    List.mapi replay.parents ~f:(fun index parent ->
      item
        ~color:(Styles.order_color index)
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
    let bg = if show_fills then Styles.blue else "#eef1f6" in
    let color = if show_fills then "#ffffff" else Styles.secondary in
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
    {%html|
      <span %{stats_style}>
        %{money_stat ~label:"Total P&L" open_pnl}
        %{money_stat ~label:"Execution benefit"
            replay.results.total_value_add_cents}
      </span>
    |}
  in
  {%html|
    <div %{row}>
      %{item ~color:Styles.blue ~line:true "TSLA price (1-min close)"}
      %{item ~color:Styles.orange ~line:true "day vwap"}
      *{order_items}
      %{stats}
      %{toggle}
    </div>
  |}
;;

(* ---------- per-order cards ---------- *)

let order_card
  (replay : Replay.t)
  ~index
  ~(parent : Replay.parent_replay)
  ~fills
  ~minute
  =
  let instruction = parent.instruction in
  let color = Styles.order_color index in
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
  let now = Replay.time_at replay ~minute in
  let status, status_color =
    if Time_ns.Ofday.( < ) now instruction.Alpha_instruction.arrival_time
    then "Pending", Styles.faint
    else if filled >= total
    then "Complete", Styles.green
    else if Time_ns.Ofday.( > ) now instruction.Alpha_instruction.deadline
    then "Expired", Styles.red
    else "Working", Styles.blue
  in
  let badge =
    Styles.s ("color:" ^ status_color ^ ";font-size:12px;font-weight:600;")
  in
  let chip =
    Styles.s
      ("display:inline-block;width:8px;height:8px;background:"
       ^ color
       ^ ";margin-right:8px;")
  in
  let title_style =
    Styles.s
      ("color:"
       ^ Styles.text
       ^ ";font-size:14px;font-weight:600;display:flex;align-items:center;")
  in
  let sub_style =
    Styles.s ("color:" ^ Styles.secondary ^ ";font-size:13px;margin-top:4px;")
  in
  let bar_outer =
    Styles.s
      "height:4px;background:#eef1f6;overflow:hidden;margin:12px 0 14px 0;"
  in
  let bar_inner =
    Styles.s
      (sprintf "height:100%%;width:%.1f%%;background:%s;" completion color)
  in
  let grid =
    Styles.s "display:grid;grid-template-columns:1fr 1fr 1fr;gap:12px 16px;"
  in
  let metric label value =
    let label_style = Styles.s Styles.label in
    {%html|
      <div>
        <div %{label_style}>#{label}</div>
        %{value}
      </div>
    |}
  in
  let plain value =
    let style =
      Styles.s
        ("color:"
         ^ Styles.text
         ^ ";font-size:14px;font-weight:600;"
         ^ Styles.mono)
    in
    {%html|<span %{style}>#{value}</span>|}
  in
  let avg = if filled = 0 then None else Some (notional // filled /. 100.) in
  let avg_view =
    plain (match avg with None -> "-" | Some a -> sprintf "$%.4f" a)
  in
  let shortfall_view =
    match avg with
    | None -> plain "-"
    | Some a ->
      bps_view
        (bps_vs
           ~side:instruction.Alpha_instruction.side
           ~avg:a
           ~benchmark:(Price.to_float parent.arrival_price))
  in
  let vwap_view =
    match avg with
    | None -> plain "-"
    | Some a ->
      bps_view
        (bps_vs
           ~side:instruction.Alpha_instruction.side
           ~avg:a
           ~benchmark:replay.vwap_by_minute.(Array.length replay.bars - 1))
  in
  let slices =
    sprintf "%d / %d" (List.length mine) (Set.length parent.order_ids)
  in
  let header =
    Styles.s "display:flex;align-items:center;justify-content:space-between;"
  in
  let title =
    sprintf
      "Order %d — %s %s TSLA"
      (index + 1)
      (side_str instruction.Alpha_instruction.side)
      (Int.to_string_hum ~delimiter:',' total)
  in
  let subtitle =
    sprintf
      "arrives %s · deadline %s · arrival price %s"
      (hhmm instruction.Alpha_instruction.arrival_time)
      (hhmm instruction.Alpha_instruction.deadline)
      (Price.to_string_dollar parent.arrival_price)
  in
  {%html|
    <div %{Styles.card "padding:16px;flex:1;min-width:300px;"}>
      <div %{header}>
        <span %{title_style}><span %{chip}></span>#{title}</span>
        <span %{badge}>#{status}</span>
      </div>
      <div %{sub_style}>#{subtitle}</div>
      <div %{bar_outer}><div %{bar_inner}></div></div>
      <div %{grid}>
        %{metric "Filled" (plain (Int.to_string_hum ~delimiter:',' filled))}
        %{metric "Slices" (plain slices)}
        %{metric "Avg fill" avg_view}
        %{metric "Shortfall" shortfall_view}
        %{metric "Vs day vwap" vwap_view}
        %{metric "Completion" (plain (sprintf "%.1f%%" completion))}
      </div>
    </div>
  |}
;;

(* ---------- fill blotter ---------- *)

let blotter (replay : Replay.t) ~fills =
  let total = Array.length replay.fills in
  let title_style =
    Styles.s ("color:" ^ Styles.text ^ ";font-size:14px;font-weight:600;")
  in
  let count_style =
    Styles.s ("color:" ^ Styles.faint ^ ";font-size:13px;font-weight:400;")
  in
  let columns = "72px 46px 64px 92px 60px 1fr" in
  let row_base = "display:grid;grid-template-columns:" ^ columns ^ ";" in
  let head_row =
    Styles.s
      (row_base
       ^ "padding:8px 16px 6px 16px;border-bottom:1px solid "
       ^ Styles.hairline
       ^ ";"
       ^ Styles.label)
  in
  let line_view (fill : Fill.t) =
    let index = Replay.parent_index_of_order replay fill.order_id in
    let style =
      Styles.s
        (row_base
         ^ "padding:2px 16px;font-size:12.5px;color:"
         ^ Styles.secondary
         ^ ";"
         ^ Styles.mono)
    in
    let order_style = Styles.s ("color:" ^ Styles.order_color index ^ ";") in
    let time = String.prefix (Time_ns.Ofday.to_string fill.time) 8 in
    let liquidity =
      match fill.liquidity with Taker -> "taker" | Maker -> "maker"
    in
    {%html|
      <div %{style}>
        <span>#{time}</span>
        <span>#{side_str fill.side}</span>
        <span>%{Size.to_int fill.size#Int}</span>
        <span>#{Price.to_string_dollar fill.price}</span>
        <span>#{liquidity}</span>
        <span %{order_style}>
          O%{Replay.parent_index_of_order replay fill.order_id + 1#Int}
          · fill %{fill.fill_id#Int}
        </span>
      </div>
    |}
  in
  let scroll =
    Styles.s
      "max-height:280px;overflow-y:auto;scrollbar-width:thin;display:flex;flex-direction:column-reverse;padding:6px \
       0;"
  in
  let header = Styles.s "padding:14px 16px 0 16px;" in
  let count = sprintf " · %d of %d fills" (List.length fills) total in
  {%html|
    <div %{Styles.card ""}>
      <div %{header}>
        <span %{title_style}>Fill blotter</span>
        <span %{count_style}>#{count}</span>
      </div>
      <div %{head_row}>
        <span>time</span>
        <span>side</span>
        <span>qty</span>
        <span>price</span>
        <span>liq</span>
        <span>order</span>
      </div>
      <div %{scroll}>*{List.map fills ~f:line_view}</div>
    </div>
  |}
;;

(* ---------- screens ---------- *)

let sim_view
  (replay : Replay.t)
  ~minute
  ~playing
  ~speed
  ~show_fills
  ~set_playing
  ~set_speed
  ~set_minute
  ~restart
  ~toggle_fills
  ~back
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
       ^ Styles.text
       ^ ";font-size:20px;font-weight:700;margin:4px 0;")
  in
  let sub_style =
    Styles.s
      ("color:" ^ Styles.secondary ^ ";font-size:13px;line-height:1.7;")
  in
  let back_style =
    Styles.s
      ("background:none;border:none;color:"
       ^ Styles.blue
       ^ ";cursor:pointer;font-size:13px;font-weight:600;padding:0;")
  in
  let cards_row =
    Styles.s "display:flex;gap:16px;flex-wrap:wrap;align-items:stretch;"
  in
  let head_row =
    Styles.s
      "display:flex;align-items:baseline;justify-content:space-between;"
  in
  let title =
    sprintf "TSLA · 2026-07-09 · %s" (String.uppercase replay.algo_name)
  in
  let command =
    sprintf
      "dune exec bin/main.exe -- examples/demo_alpha_tsla.csv TSLA \
       2026-07-09 %s"
      replay.algo_name
  in
  {%html|
    <div %{page}>
      <div>
        <div %{head_row}>
          <span %{Styles.brand}>execlab</span>
          <button %{back_style} on_click=%{fun _ -> back}>
            ← New simulation
          </button>
        </div>
        <div %{title_style}>#{title}</div>
        <div %{sub_style}>
          source: <span %{Styles.code_chip}>#{command}</span>
        </div>
      </div>
      %{controls replay ~minute ~playing ~speed ~set_playing ~set_speed
          ~set_minute ~restart}
      <div %{Styles.card "padding-bottom:8px;"}>
        %{legend replay ~minute ~fills ~show_fills ~toggle_fills}
        %{chart replay ~minute ~fills ~show_fills}
      </div>
      <div %{cards_row}>
        *{List.mapi replay.parents ~f:(fun index parent ->
            order_card replay ~index ~parent ~fills ~minute)}
      </div>
      %{blotter replay ~fills}
    </div>
  |}
;;

let algo_pill ~selected ~on_click label =
  let bg = if selected then Styles.blue else "#eef1f6" in
  let color = if selected then "#ffffff" else Styles.secondary in
  let style =
    Styles.s
      ("background:"
       ^ bg
       ^ ";color:"
       ^ color
       ^ ";border:none;border-radius:5px;padding:8px \
          18px;cursor:pointer;font-size:13px;font-weight:700;")
  in
  {%html|<button %{style} on_click=%{on_click}>#{label}</button>|}
;;

let setup_view ~algo ~set_algo ~start =
  let page =
    Styles.s
      "display:flex;flex-direction:column;gap:16px;max-width:640px;margin:64px \
       auto;padding:20px;"
  in
  let title_style =
    Styles.s
      ("color:"
       ^ Styles.text
       ^ ";font-size:20px;font-weight:700;margin:4px 0;")
  in
  let sub_style =
    Styles.s ("color:" ^ Styles.secondary ^ ";font-size:14px;")
  in
  let section_label = Styles.s (Styles.label ^ "margin-bottom:8px;") in
  let pills = Styles.s "display:flex;gap:8px;" in
  let start_style =
    Styles.s
      ("background:"
       ^ Styles.blue
       ^ ";color:#ffffff;border:none;border-radius:5px;padding:10px \
          20px;cursor:pointer;font-size:14px;font-weight:700;align-self:flex-start;"
      )
  in
  let instruction_row (instruction : Alpha_instruction.t) =
    let row =
      Styles.s
        ("display:flex;gap:16px;padding:8px 0;border-bottom:1px solid "
         ^ Styles.hairline
         ^ ";font-size:13px;color:"
         ^ Styles.text
         ^ ";"
         ^ Styles.mono)
    in
    let side_style = Styles.s "font-weight:700;width:44px;" in
    let dim = Styles.s ("color:" ^ Styles.secondary ^ ";") in
    let qty =
      sprintf
        "%s TSLA"
        (Int.to_string_hum ~delimiter:',' (Size.to_int instruction.quantity))
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
  in
  {%html|
    <div %{page}>
      <div>
        <span %{Styles.brand}>execlab</span>
        <div %{title_style}>New simulation</div>
        <div %{sub_style}>TSLA · 2026-07-09 · bar-based fill model</div>
      </div>
      <div %{Styles.card "padding:16px;"}>
        <div %{section_label}>Execution algorithm</div>
        <div %{pills}>
          %{algo_pill ~selected:(String.equal algo "twap")
              ~on_click:(fun _ -> set_algo "twap") "TWAP"}
          %{algo_pill ~selected:(String.equal algo "immediate")
              ~on_click:(fun _ -> set_algo "immediate") "Immediate"}
        </div>
      </div>
      <div %{Styles.card "padding:16px;"}>
        <div %{section_label}>Alpha instructions</div>
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
  let show_fills, set_show_fills = Bonsai.state false graph in
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
    let%bind.Effect () = set_minute (fun (_ : int) -> 0) in
    let%bind.Effect () = set_playing true in
    set_screen Screen.Sim
  in
  let restart =
    let%arr set_minute and set_playing in
    let%bind.Effect () = set_minute (fun (_ : int) -> 0) in
    set_playing true
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
  and show_fills
  and set_show_fills
  and set_minute
  and set_screen
  and start
  and restart in
  let body =
    match screen, replay with
    | Screen.Sim, Some r ->
      sim_view
        r
        ~minute
        ~playing
        ~speed
        ~show_fills
        ~set_playing
        ~set_speed
        ~set_minute
        ~restart
        ~toggle_fills:(set_show_fills (not show_fills))
        ~back:(set_screen Screen.Setup)
    | Setup, _ | Sim, None -> setup_view ~algo ~set_algo ~start
  in
  let shell =
    Styles.s
      ("min-height:100vh;background:"
       ^ Styles.page_bg
       ^ ";font-family:system-ui,-apple-system,'Segoe UI',sans-serif;")
  in
  {%html|<div %{shell}>%{body}</div>|}
;;

let () = Bonsai_web.Start.start app
