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

(* ---------- pure views ---------- *)

let header_bar
  ~clock
  ~progress_pct
  ~complete
  ~playing
  ~speed
  ~set_playing
  ~set_speed
  ~back
  =
  let btn ~active ~on_click label =
    let bg = if active then Styles.accent_soft else "transparent" in
    let color = if active then Styles.accent else Styles.text_dim in
    let style =
      Styles.s
        [%string
          "background:%{bg};color:%{color};border:%{Styles.border};border-radius:6px;padding:4px \
           10px;cursor:pointer;font-size:12px;font-weight:600;"]
    in
    {%html|<button %{style} on_click=%{on_click}>#{label}</button>|}
  in
  let progress_outer =
    Styles.s
      [%string
        "flex:1;height:6px;background:%{Styles.bg2};border-radius:9999px;overflow:hidden;min-width:120px;"]
  in
  let progress_inner =
    Styles.s
      [%string
        "height:100%%;width:%{progress_pct#Int}%%;background:%{Styles.accent};"]
  in
  let clock_style =
    Styles.s
      ([%string "color:%{Styles.text};font-size:20px;font-weight:700;"]
       ^ Styles.mono)
  in
  let badge =
    if complete
    then (
      let style =
        Styles.s
          [%string
            "color:%{Styles.green};border:1px solid \
             %{Styles.green};border-radius:9999px;padding:2px \
             10px;font-size:11px;font-weight:700;"]
      in
      {%html|<span %{style}>COMPLETE</span>|})
    else Vdom.Node.none
  in
  let bar =
    Styles.s
      [%string
        "display:flex;align-items:center;gap:14px;background:%{Styles.bg1};border:%{Styles.border};border-radius:10px;padding:10px \
         14px;"]
  in
  let title =
    Styles.s
      [%string
        "color:%{Styles.accent};font-weight:800;letter-spacing:0.06em;font-size:14px;"]
  in
  let chip =
    Styles.s
      [%string
        "color:%{Styles.text_dim};background:%{Styles.bg2};border-radius:6px;padding:3px \
         8px;font-size:12px;"]
  in
  {%html|
    <div %{bar}>
      <span %{title}>EXECLAB</span>
      <span %{chip}>TSLA · 2026-07-09</span>
      <span %{clock_style}>#{clock}</span>
      <div %{progress_outer}><div %{progress_inner}></div></div>
      %{badge}
      %{btn ~active:false ~on_click:(fun _ -> set_playing (not playing))
          (if playing then "Pause" else "Play")}
      %{btn ~active:(speed = 1) ~on_click:(fun _ -> set_speed 1) "1x"}
      %{btn ~active:(speed = 4) ~on_click:(fun _ -> set_speed 4) "4x"}
      %{btn ~active:(speed = 16) ~on_click:(fun _ -> set_speed 16) "16x"}
      %{btn ~active:false ~on_click:(fun _ -> back) "Setup"}
    </div>
  |}
;;

let price_chart (replay : Replay.t) ~minute =
  let bars = replay.bars in
  let n = Array.length bars in
  let closes =
    Array.map bars ~f:(fun bar -> Price.to_float bar.Market_bar.close)
  in
  let lo = Array.min_elt closes ~compare:Float.compare |> Option.value_exn in
  let hi = Array.max_elt closes ~compare:Float.compare |> Option.value_exn in
  let w = 860. in
  let h = 240. in
  let x i = Float.of_int i /. Float.of_int (n - 1) *. w in
  let y v = h -. ((v -. lo) /. (hi -. lo) *. (h -. 16.)) -. 8. in
  let pts =
    List.init (minute + 1) ~f:(fun i ->
      [%string "%{x i#Float},%{y closes.(i)#Float}"])
    |> String.concat ~sep:" "
  in
  let area = [%string "%{pts} %{x minute#Float},%{h#Float} 0,%{h#Float}"] in
  let svg name attrs children = Vdom.Node.create_svg name ~attrs children in
  let attr = Vdom.Attr.create in
  svg
    "svg"
    [ attr "viewBox" [%string "0 0 %{w#Float} %{h#Float}"]
    ; Styles.s "width:100%;height:240px;display:block;"
    ]
    [ svg
        "polygon"
        [ attr "points" area; attr "fill" "rgba(139,92,246,0.18)" ]
        []
    ; svg
        "polyline"
        [ attr "points" pts
        ; attr "fill" "none"
        ; attr "stroke" Styles.accent
        ; attr "stroke-width" "2"
        ]
        []
    ]
;;

let parent_panel rows =
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
      | _ -> Styles.text_faint
    in
    let pct = if row.total = 0 then 0 else row.filled * 100 / row.total in
    let cell = Styles.s ("color:" ^ Styles.text ^ ";" ^ Styles.mono) in
    let side_style =
      Styles.s [%string "color:%{side_color};font-weight:700;"]
    in
    let status_style =
      Styles.s
        [%string "color:%{status_color};font-size:11px;font-weight:700;"]
    in
    let bar_outer =
      Styles.s
        [%string
          "width:120px;height:5px;background:%{Styles.bg2};border-radius:9999px;overflow:hidden;"]
    in
    let bar_inner =
      Styles.s
        [%string
          "height:100%%;width:%{pct#Int}%%;background:%{Styles.accent};"]
    in
    let tr =
      Styles.s
        "display:flex;align-items:center;gap:12px;padding:7px \
         0;border-bottom:1px solid rgba(255,255,255,0.05);"
    in
    {%html|
      <div %{tr}>
        <span %{side_style}>#{side}</span>
        <span %{cell}>%{row.filled#Int} / %{row.total#Int}</span>
        <div %{bar_outer}><div %{bar_inner}></div></div>
        <span %{status_style}>#{row.status}</span>
      </div>
    |}
  in
  {%html|
    <div %{Styles.panel ""}>
      <div %{Styles.label}>Parent orders</div>
      *{List.map rows ~f:row_view}
    </div>
  |}
;;

let fills_panel (fills : Fill.t list) =
  let recent = List.rev fills |> fun l -> List.take l 14 in
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
        ("display:flex;align-items:center;gap:10px;padding:5px \
          0;border-bottom:1px solid \
          rgba(255,255,255,0.05);font-size:12px;color:"
         ^ Styles.text
         ^ ";"
         ^ Styles.mono)
    in
    let side_style =
      Styles.s [%string "color:%{side_color};font-weight:700;width:12px;"]
    in
    let dim = Styles.s ("color:" ^ Styles.text_dim ^ ";") in
    {%html|
      <div %{tr}>
        <span %{dim}>#{time}</span>
        <span %{side_style}>#{side}</span>
        <span>%{Size.to_int fill.size#Int}</span>
        <span>#{Price.to_string_dollar fill.price}</span>
        <span %{Styles.dot liq_color}></span>
      </div>
    |}
  in
  let empty =
    let style =
      Styles.s
        ("color:" ^ Styles.text_faint ^ ";font-size:12px;padding:8px 0;")
    in
    {%html|<div %{style}>No fills yet</div>|}
  in
  {%html|
    <div %{Styles.panel "flex:1;"}>
      <div %{Styles.label}>Recent fills</div>
      %{if List.is_empty recent then empty else Vdom.Node.none}
      *{List.map recent ~f:row_view}
    </div>
  |}
;;

let stats_panel (replay : Replay.t) ~fills ~minute =
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
  let price_now =
    Price.to_string_dollar replay.bars.(minute).Market_bar.close
  in
  let stat label value =
    let value_style =
      Styles.s
        ([%string "color:%{Styles.text};font-size:18px;font-weight:700;"]
         ^ Styles.mono)
    in
    {%html|
      <div>
        <div %{Styles.label}>#{label}</div>
        <div %{value_style}>#{value}</div>
      </div>
    |}
  in
  let grid =
    Styles.s "display:grid;grid-template-columns:1fr 1fr;gap:12px;"
  in
  {%html|
    <div %{Styles.panel ""}>
      <div %{grid}>
        %{stat "Last price" price_now}
        %{stat "Algo" replay.algo_name}
        %{stat "Shares filled" (Int.to_string_hum ~delimiter:',' shares)}
        %{stat "Avg fill" avg}
      </div>
    </div>
  |}
;;

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
  let rows = Replay.parent_rows replay ~minute in
  let layout =
    Styles.s
      "display:grid;grid-template-columns:1fr \
       320px;gap:14px;align-items:start;"
  in
  let left = Styles.s "display:flex;flex-direction:column;gap:14px;" in
  let right = Styles.s "display:flex;flex-direction:column;gap:14px;" in
  let page =
    Styles.s
      "display:flex;flex-direction:column;gap:14px;max-width:1240px;margin:0 \
       auto;padding:18px;"
  in
  {%html|
    <div %{page}>
      %{header_bar ~clock:(Replay.clock_string replay ~minute)
          ~progress_pct:(minute * 100 / last)
          ~complete:(minute >= last) ~playing ~speed ~set_playing ~set_speed
          ~back}
      <div %{layout}>
        <div %{left}>
          <div %{Styles.panel ""}>
            <div %{Styles.label}>TSLA · price</div>
            %{price_chart replay ~minute}
          </div>
          %{parent_panel rows}
        </div>
        <div %{right}>
          %{stats_panel replay ~fills ~minute}
          %{fills_panel fills}
        </div>
      </div>
    </div>
  |}
;;

let algo_card ~selected ~on_click ~name ~blurb =
  let border =
    if selected
    then [%string "1px solid %{Styles.accent}"]
    else Styles.border
  in
  let bg = if selected then Styles.accent_soft else Styles.bg1 in
  let style =
    Styles.s
      [%string
        "background:%{bg};border:%{border};border-radius:10px;padding:16px;cursor:pointer;flex:1;"]
  in
  let name_style =
    Styles.s [%string "color:%{Styles.text};font-weight:700;font-size:15px;"]
  in
  let blurb_style =
    Styles.s
      [%string
        "color:%{Styles.text_dim};font-size:12px;margin-top:6px;line-height:1.5;"]
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
      "display:flex;flex-direction:column;gap:16px;max-width:760px;margin:40px \
       auto;padding:18px;"
  in
  let title =
    Styles.s
      [%string
        "color:%{Styles.text};font-size:24px;font-weight:800;letter-spacing:0.02em;"]
  in
  let subtitle =
    Styles.s [%string "color:%{Styles.text_faint};font-size:13px;"]
  in
  let cards = Styles.s "display:flex;gap:14px;" in
  let start_style =
    Styles.s
      [%string
        "background:%{Styles.accent};color:#ffffff;border:none;border-radius:8px;padding:12px \
         22px;font-size:14px;font-weight:700;cursor:pointer;align-self:flex-start;"]
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
        ("display:flex;gap:14px;padding:6px 0;font-size:13px;color:"
         ^ Styles.text
         ^ ";"
         ^ Styles.mono)
    in
    let side_style =
      Styles.s [%string "color:%{side_color};font-weight:700;width:38px;"]
    in
    {%html|
      <div %{tr}>
        <span %{side_style}>#{side}</span>
        <span>%{Size.to_int instruction.quantity#Int} TSLA</span>
        <span>#{arrival} -> #{deadline}</span>
      </div>
    |}
  in
  {%html|
    <div %{page}>
      <div>
        <div %{title}>New simulation</div>
        <div %{subtitle}>TSLA · 2026-07-09 · bar-based fill model</div>
      </div>
      <div %{cards}>
        %{algo_card ~selected:(String.equal algo "twap")
            ~on_click:(fun _ -> set_algo "twap") ~name:"TWAP"
            ~blurb:"Slices the order evenly across the instruction window; \
                    always on schedule, catches up automatically."}
        %{algo_card ~selected:(String.equal algo "immediate")
            ~on_click:(fun _ -> set_algo "immediate") ~name:"Immediate"
            ~blurb:"The naive baseline: the full order as one market order \
                    the moment the instruction arrives."}
      </div>
      <div %{Styles.panel ""}>
        <div %{Styles.label}>Alpha instructions</div>
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
      [%string
        "min-height:100vh;background:%{Styles.bg0};color-scheme:dark;font-family:system-ui,sans-serif;"]
  in
  {%html|<div %{shell}>%{body}</div>|}
;;

let () = Bonsai_web.Start.start app
