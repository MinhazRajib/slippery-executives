(* The six screens of the wizard: Dashboard, Choose_day, Alpha, Algo,
   Confirm, Results. Every screen is a Bonsai component over the single
   {!Model.t}: it renders a view of the model and navigates by functional
   update. The run itself happens in the Confirm screen's click effect via
   {!Sim.run}. *)

open! Core
open! Execlab_types
open! Execlab_market
open! Execlab_alpha
open! Execlab_simulation
open! Bonsai_web
open Bonsai.Let_syntax

(* ---------- shared event/attr shorthands ---------- *)

let on_click effect =
  Vdom.Attr.on_click (fun (_ : _ Js_of_ocaml.Js.t) -> effect)
;;

let on_input f =
  Vdom.Attr.on_input (fun (_ : _ Js_of_ocaml.Js.t) text -> f text)
;;

let value_is text = Vdom.Attr.string_property "value" text

let disabled_if condition =
  if condition then Vdom.Attr.disabled else Vdom.Attr.empty
;;

let goto screen (update : Model.updater) =
  update (fun m -> { m with Model.screen })
;;

(* A hover explainer (native tooltip) for metric names. *)
let help text = Vdom.Attr.create "title" text

(* ---------- shared views ---------- *)

module Side_badge = struct
  let view side =
    match (side : Side.t) with
    | Buy -> {%html|<span class="badge buy">BUY</span>|}
    | Sell -> {%html|<span class="badge sell">SELL</span>|}
  ;;
end

module Tile = struct
  (* One headline stat. [tone]'s sign colors the value; use it for signed
     quantities only. [help] adds a hover explainer. *)
  let view ?(hero = false) ?tone ?help:help_text ~label ~value ~sub () =
    let value_class =
      String.concat
        ~sep:" "
        ([ "tile-value" ]
         @ (if hero then [ "hero" ] else [])
         @
         match tone with
         | None -> []
         | Some tone -> [ (if tone >= 0 then "good" else "bad") ])
    in
    let help_attr =
      match help_text with
      | None -> Vdom.Attr.empty
      | Some text ->
        Vdom.Attr.many [ help text; Vdom.Attr.class_ "has-help" ]
    in
    {%html|
      <div class="tile" %{help_attr}>
        <div class="tile-label">#{label}</div>
        <div class=%{value_class}>#{value}</div>
        <div class="tile-sub">#{sub}</div>
      </div>
    |}
  ;;
end

(* ---------- alpha parsing + run plumbing ---------- *)

let sample_alpha symbol =
  let s = Symbol.to_string symbol in
  String.concat
    ~sep:"\n"
    [ [%string "09:45:00,%{s},BUY,20000,11:00:00"]
    ; [%string "11:15:00,%{s},SELL,8000,12:30:00"]
    ; [%string "13:30:00,%{s},BUY,12000,15:30:00"]
    ]
;;

(* Row-level validation is the parser's; the context check — every
   instruction must trade the chosen day's symbol — lives here, where the run
   config is known. *)
let parse_alpha ~alpha_text ~symbol : Alpha_instruction.t list Or_error.t =
  let open Or_error.Let_syntax in
  let%bind ({ instructions } : Parser.t) = Parser.parse alpha_text in
  let%bind () =
    if List.is_empty instructions
    then Or_error.error_string "the alpha file has no instructions"
    else Ok ()
  in
  let mismatched =
    List.filter_mapi instructions ~f:(fun index instruction ->
      let instruction_symbol = instruction.Alpha_instruction.symbol in
      if Symbol.equal instruction_symbol symbol
      then None
      else
        Some
          (Error.create_s
             [%message
               "instruction is not for this run's symbol"
                 (index : int)
                 (instruction_symbol : Symbol.t)
                 ~run_symbol:(symbol : Symbol.t)]))
  in
  match mismatched with
  | [] -> Ok instructions
  | _ :: _ -> Error (Error.of_list mismatched)
;;

(* js_of_ocaml integers are 32-bit, so the analytics' cents arithmetic
   overflows once a single instruction's notional passes ~$21M. Refuse
   anything near that, with headroom, rather than silently corrupting the
   grading. *)
let max_safe_notional_dollars = 15_000_000.

let validate_notional ~(day : Trading_day.t) instructions =
  let session_high =
    List.map day.bars ~f:(fun bar -> Price.to_float bar.high)
    |> List.max_elt ~compare:Float.compare
    |> Option.value ~default:0.
  in
  let too_big =
    List.filter_mapi
      instructions
      ~f:(fun index (instruction : Alpha_instruction.t) ->
        let notional = Size.to_float instruction.quantity *. session_high in
        if Float.(notional > max_safe_notional_dollars)
        then
          Some
            (Error.create_s
               [%message
                 "instruction notional is too large for the browser build \
                  (32-bit integers): keep quantity x session-high price \
                  under $15M"
                   (index : int)
                   ~quantity:(instruction.quantity : Size.t)
                   ~approx_notional_dollars:(notional : float)])
        else None)
  in
  match too_big with [] -> Ok () | _ :: _ -> Error (Error.of_list too_big)
;;

let fill_config_of_model (model : Model.t) : Fill_model.Config.t Or_error.t =
  Or_error.try_with (fun () ->
    let dollars ~name text =
      let value = Float.of_string (String.strip text) in
      if Float.(value < 0.) then failwith [%string "%{name} must be >= 0"];
      Price.of_float_round_nearest value
    in
    let half_spread = dollars ~name:"half-spread" model.half_spread_text in
    let impact_coefficient = dollars ~name:"impact" model.impact_text in
    let max_participation =
      Float.of_string (String.strip model.participation_text)
    in
    if Float.(max_participation <= 0.) || Float.(max_participation > 1.)
    then failwith "participation must be within (0, 1]";
    { Fill_model.Config.half_spread; max_participation; impact_coefficient })
;;

let run_from_model (model : Model.t)
  : (Sim.Output.t * Model.Run_record.t) Or_error.t
  =
  let open Or_error.Let_syntax in
  let%bind symbol, date =
    match model.selection with
    | Some selection -> Ok selection
    | None -> Or_error.error_string "no market day selected"
  in
  let%bind day = Dataset.load ~symbol ~date in
  let%bind instructions = parse_alpha ~alpha_text:model.alpha_text ~symbol in
  let%bind () = validate_notional ~day instructions in
  let%bind fill_config = fill_config_of_model model in
  let%bind output =
    Sim.run ~day ~instructions ~fill_config ~algo:model.algo
  in
  let record =
    { Model.Run_record.symbol
    ; date
    ; algo_name = output.algo_name
    ; alpha_capture = Sim.Totals.alpha_capture output.totals
    ; value_add_cents = output.totals.value_add_cents
    ; net_cents = output.totals.net_cents
    }
  in
  Ok (output, record)
;;

(* ---------- Dashboard ---------- *)

module Dashboard = struct
  let run_row (run : Model.Run_record.t) =
    let capture =
      match run.alpha_capture with
      | None -> "—"
      | Some capture -> Fmt.pct capture
    in
    let tone = if run.value_add_cents >= 0 then "num good" else "num bad" in
    {%html|
      <tr>
        <td class="mono">#{Symbol.to_string run.symbol}</td>
        <td class="mono muted">#{Fmt.date run.date}</td>
        <td>#{run.algo_name}</td>
        <td class="num">#{capture}</td>
        <td class=%{tone}>#{Fmt.signed_cents run.value_add_cents}</td>
        <td class="num">#{Fmt.signed_cents run.net_cents}</td>
      </tr>
    |}
  ;;

  let runs_table (runs : Model.Run_record.t list) ~empty_hint =
    match runs with
    | [] -> {%html|<div class="muted small">#{empty_hint}</div>|}
    | _ :: _ ->
      let rows = List.map runs ~f:run_row in
      {%html|
        <table class="table">
          <thead>
            <tr>
              <th>Sym</th><th>Date</th><th>Algo</th>
              <th class="num">Captured</th>
              <th class="num">Value add</th>
              <th class="num">Net P&L</th>
            </tr>
          </thead>
          <tbody>*{rows}</tbody>
        </table>
      |}
  ;;

  let component
    ~(model : Model.t Bonsai.t)
    ~(update : Model.updater Bonsai.t)
    (local_ _graph)
    =
    let%arr { Model.runs; _ } = model
    and update in
    let day_count =
      List.sum (module Int) Dataset.catalog ~f:(fun (_, dates) ->
        List.length dates)
    in
    (* Land on the day picker with a day already previewed. *)
    let start =
      on_click
        (update (fun m ->
           let selection =
             match m.Model.selection with
             | Some _ as selection -> selection
             | None ->
               let symbol = List.hd_exn Dataset.symbols in
               Some (symbol, List.hd_exn (Dataset.dates_for symbol))
           in
           { m with Model.screen = Model.Screen.Choose_day; selection }))
    in
    let best =
      List.filter runs ~f:(fun run -> Option.is_some run.alpha_capture)
      |> List.sort
           ~compare:
             (Comparable.lift
                [%compare: float option]
                ~f:(fun (run : Model.Run_record.t) -> run.alpha_capture))
      |> List.rev
    in
    let best = List.take best 5 in
    let recent = List.take runs 8 in
    let tiles =
      [ Tile.view
          ~label:"Symbols"
          ~value:(Int.to_string (List.length Dataset.symbols))
          ~sub:
            (String.concat
               ~sep:" "
               (List.map Dataset.symbols ~f:Symbol.to_string))
          ()
      ; Tile.view
          ~label:"Sessions"
          ~value:(Int.to_string day_count)
          ~sub:"real 1-minute OHLCV bars"
          ()
      ; Tile.view
          ~label:"Bars per session"
          ~value:"390"
          ~sub:"09:30 – 15:59 exchange time"
          ()
      ]
    in
    {%html|
      <div class="content">
        <h1 class="page-title">ExecLab</h1>
        <p class="page-sub">
          Your alpha model already decided <em>what</em> to trade. Upload its
          instructions, execute them against a real historical session with a
          configurable algorithm, and measure how much of the theoretical
          alpha survives execution costs.
        </p>
        <button class="btn primary big" %{start}>Run a simulation</button>
        <div class="section-label">Bundled market data</div>
        <div class="row">*{tiles}</div>
        <div class="row mt-lg">
          <div class="grow">
            <div class="section-label">Recent runs</div>
            <div class="panel">
              %{runs_table recent ~empty_hint:"No runs yet — run a simulation to populate this."}
            </div>
          </div>
          <div class="grow">
            <div class="section-label">Best runs (alpha captured)</div>
            <div class="panel">
              %{runs_table best ~empty_hint:"Best runs appear once a run has positive gross alpha."}
            </div>
          </div>
        </div>
      </div>
    |}
  ;;
end

(* ---------- Choose a market day ---------- *)

module Choose_day = struct
  let component
    ~(model : Model.t Bonsai.t)
    ~(update : Model.updater Bonsai.t)
    (local_ graph)
    =
    let selection =
      let%arr { Model.selection; _ } = model in
      selection
    in
    let day =
      let%arr selection in
      Option.bind selection ~f:(fun (symbol, date) ->
        Or_error.ok (Dataset.load ~symbol ~date))
    in
    let chart = Chart.component ~day ~fills:(Bonsai.return []) graph in
    let%arr selection and day and chart and update in
    let symbol_buttons =
      List.map Dataset.symbols ~f:(fun symbol ->
        let selected =
          match selection with
          | Some (s, _) -> Symbol.equal s symbol
          | None -> false
        in
        let classes =
          Vdom.Attr.classes
            ([ "seg-btn" ] @ if selected then [ "selected" ] else [])
        in
        let pick =
          on_click
            (update (fun m ->
               let date =
                 match m.Model.selection with
                 | Some (s, date)
                   when Symbol.equal s symbol
                        && List.mem
                             (Dataset.dates_for symbol)
                             date
                             ~equal:Date.equal ->
                   date
                 | Some _ | None -> List.hd_exn (Dataset.dates_for symbol)
               in
               { m with Model.selection = Some (symbol, date) }))
        in
        {%html|<button %{classes} %{pick}>#{Symbol.to_string symbol}</button>|})
    in
    let date_chips =
      match selection with
      | None ->
        {%html|<div class="muted small">Pick a symbol to list its sessions.</div>|}
      | Some (symbol, selected_date) ->
        let chips =
          List.map (Dataset.dates_for symbol) ~f:(fun date ->
            let selected = Date.equal date selected_date in
            let classes =
              Vdom.Attr.classes
                ([ "chip" ] @ if selected then [ "selected" ] else [])
            in
            let pick =
              on_click
                (update (fun m ->
                   { m with Model.selection = Some (symbol, date) }))
            in
            {%html|<button %{classes} %{pick}>#{Fmt.date date}</button>|})
        in
        {%html|<div class="row">*{chips}</div>|}
    in
    let stats =
      match day with
      | None -> Vdom.Node.none
      | Some day ->
        let last_bar = List.last_exn day.Trading_day.bars in
        let volatility = Day_stats.realized_volatility day in
        let tiles =
          [ Tile.view
              ~label:"Last close"
              ~value:(Fmt.price last_bar.Market_bar.close)
              ~sub:"15:59 bar"
              ()
          ; Tile.view
              ~label:"Day VWAP"
              ~value:(Fmt.dollars_4dp (Day_stats.vwap day))
              ~sub:"typical-price weighted"
              ()
          ; Tile.view
              ~label:"Volume"
              ~value:(Fmt.shares (Day_stats.total_volume day))
              ~sub:"shares traded"
              ()
          ; Tile.view
              ~label:"Realized vol"
              ~value:(sprintf "%.2f%%" (volatility *. 100.))
              ~sub:"daily, from 1-min returns"
              ()
          ]
        in
        {%html|<div class="row">*{tiles}</div>|}
    in
    let continue_button =
      let continue_ =
        on_click
          (update (fun m ->
             match m.Model.selection with
             | None -> m
             | Some (symbol, _) ->
               let alpha_text =
                 if String.is_empty (String.strip m.Model.alpha_text)
                 then sample_alpha symbol
                 else m.Model.alpha_text
               in
               { m with Model.screen = Model.Screen.Alpha; alpha_text }))
      in
      let disabled = disabled_if (Option.is_none day) in
      {%html|<button class="btn primary" %{continue_} %{disabled}>Continue with this day</button>|}
    in
    let surprise =
      on_click
        (update (fun m ->
           let symbol = List.random_element_exn Dataset.symbols in
           let date = List.random_element_exn (Dataset.dates_for symbol) in
           { m with Model.selection = Some (symbol, date) }))
    in
    {%html|
      <div class="content wide">
        <h1 class="page-title">Choose a market day</h1>
        <p class="page-sub">
          Every later step is validated against this session. Only bundled
          (symbol, date) pairs are offered — the lab replays real one-minute
          bars from that day.
        </p>
        <div class="section-label">Symbol</div>
        <div class="row">
          <div class="seg">*{symbol_buttons}</div>
          <button class="btn ghost" %{surprise}>🎲 Surprise me</button>
        </div>
        <div class="section-label">Session</div>
        %{date_chips}
        <div class="section-label">Preview</div>
        %{stats}
        <div class="mt-md">%{chart}</div>
        <div class="mt-xl">%{continue_button}</div>
      </div>
    |}
  ;;
end

(* ---------- Upload / edit the alpha CSV ---------- *)

module Alpha_screen = struct
  let instruction_row index (instruction : Alpha_instruction.t) ~delete =
    let window_minutes =
      Float.to_int
        (Time_ns.Span.to_min
           (Time_ns.Ofday.diff instruction.deadline instruction.arrival_time))
    in
    {%html|
      <tr>
        <td class="num muted">#{Int.to_string (index + 1)}</td>
        <td class="mono">#{Fmt.ofday instruction.arrival_time}</td>
        <td>%{Side_badge.view instruction.side}</td>
        <td class="num">#{Fmt.shares instruction.quantity}</td>
        <td class="mono">#{Fmt.ofday instruction.deadline}</td>
        <td class="num muted">#{Int.to_string window_minutes} min</td>
        <td><button class="row-delete" title="Remove this instruction" %{on_click delete}>✕</button></td>
      </tr>
    |}
  ;;

  (* The "Add instruction" mini-form: friendlier than editing CSV by hand. It
     appends a CSV line, so the parser preview stays the single source of
     validation feedback. *)
  let builder
    ~symbol
    ~side
    ~set_side
    ~qty
    ~set_qty
    ~arrival
    ~set_arrival
    ~deadline
    ~set_deadline
    ~(update : Model.updater)
    =
    (* <input type="time"> yields HH:MM (sometimes HH:MM:SS). *)
    let with_seconds time =
      if String.length time = 5 then time ^ ":00" else time
    in
    let add =
      on_click
        (update (fun m ->
           match m.Model.selection with
           | None -> m
           | Some ((_ : Symbol.t), (_ : Date.t)) ->
             let line =
               String.concat
                 ~sep:","
                 [ with_seconds arrival
                 ; symbol
                 ; side
                 ; String.strip qty
                 ; with_seconds deadline
                 ]
             in
             let text = String.strip m.Model.alpha_text in
             let alpha_text =
               if String.is_empty text then line else text ^ "\n" ^ line
             in
             { m with Model.alpha_text }))
    in
    let side_select =
      let option value =
        let selected =
          if String.equal value side
          then Vdom.Attr.create "selected" "selected"
          else Vdom.Attr.empty
        in
        {%html|<option value=%{value} %{selected}>#{value}</option>|}
      in
      {%html|
        <select
          class="input"
          %{Vdom.Attr.on_change (fun (_ : _ Js_of_ocaml.Js.t) v -> set_side v)}>
          %{option "BUY"}
          %{option "SELL"}
        </select>
      |}
    in
    let time_field ~label ~value ~set =
      {%html|
        <label class="field">
          <div class="field-label">#{label}</div>
          <input
            class="input"
            type="time"
            %{value_is value}
            %{on_input set} />
        </label>
      |}
    in
    {%html|
      <div class="panel builder">
        <label class="field">
          <div class="field-label">Side</div>
          %{side_select}
        </label>
        <label class="field">
          <div class="field-label">Quantity</div>
          <input class="input num" %{value_is qty} %{on_input set_qty} />
        </label>
        %{time_field ~label:"Arrival" ~value:arrival ~set:set_arrival}
        %{time_field ~label:"Deadline" ~value:deadline ~set:set_deadline}
        <button class="btn" %{add}>+ Add instruction</button>
      </div>
    |}
  ;;

  let component
    ~(model : Model.t Bonsai.t)
    ~(update : Model.updater Bonsai.t)
    (local_ graph)
    =
    let side, set_side = Bonsai.state "BUY" graph in
    let qty, set_qty = Bonsai.state "10000" graph in
    let arrival, set_arrival = Bonsai.state "10:00" graph in
    let deadline, set_deadline = Bonsai.state "12:00" graph in
    let parsed =
      let%arr { Model.alpha_text; selection; _ } = model in
      match selection with
      | None -> Or_error.error_string "no market day selected"
      | Some (symbol, date) ->
        let open Or_error.Let_syntax in
        let%bind instructions = parse_alpha ~alpha_text ~symbol in
        let%bind day = Dataset.load ~symbol ~date in
        let%bind () = validate_notional ~day instructions in
        Ok instructions
    in
    let%arr parsed
    and { Model.alpha_text; selection; _ } = model
    and update
    and side
    and set_side
    and qty
    and set_qty
    and arrival
    and set_arrival
    and deadline
    and set_deadline in
    let symbol =
      match selection with
      | Some (symbol, _) -> Symbol.to_string symbol
      | None -> "?"
    in
    let set_text text =
      update (fun m -> { m with Model.alpha_text = text })
    in
    let insert_sample =
      on_click
        (update (fun m ->
           match m.Model.selection with
           | None -> m
           | Some (symbol, _) ->
             { m with Model.alpha_text = sample_alpha symbol }))
    in
    let clear =
      on_click (update (fun m -> { m with Model.alpha_text = "" }))
    in
    let delete_line index =
      update (fun m ->
        let lines = String.split_lines (String.strip m.Model.alpha_text) in
        let lines =
          List.filteri lines ~f:(fun i (_ : string) -> i <> index)
        in
        { m with Model.alpha_text = String.concat ~sep:"\n" lines })
    in
    let preview =
      if String.is_empty (String.strip alpha_text)
      then
        {%html|
          <div class="callout">
            Build instructions with the form on the left, paste your alpha
            model's CSV (one instruction per line, no header:
            <span class="mono">arrival,symbol,side,quantity,deadline</span>),
            or start from the sample.
          </div>
        |}
      else (
        match parsed with
        | Error error ->
          {%html|
            <div class="callout error">
              Can't use this file yet:
              <pre>#{Error.to_string_hum error}</pre>
            </div>
          |}
        | Ok instructions ->
          let rows =
            List.mapi instructions ~f:(fun index instruction ->
              instruction_row index instruction ~delete:(delete_line index))
          in
          let total =
            List.sum (module Int) instructions ~f:(fun i ->
              Size.to_int i.quantity)
          in
          {%html|
            <div>
              <div class="callout ok">
                #{Int.to_string (List.length instructions)} instructions ·
                #{Fmt.shares_int total} shares · all for #{symbol}
              </div>
              <div class="panel mt-md">
                <table class="table">
                  <thead>
                    <tr>
                      <th class="num">#</th><th>Arrival</th><th>Side</th>
                      <th class="num">Quantity</th><th>Deadline</th>
                      <th class="num">Window</th><th></th>
                    </tr>
                  </thead>
                  <tbody>*{rows}</tbody>
                </table>
              </div>
            </div>
          |})
    in
    let back = on_click (goto Model.Screen.Choose_day update) in
    let continue_ = on_click (goto Model.Screen.Algo update) in
    let continue_disabled = disabled_if (Or_error.is_error parsed) in
    {%html|
      <div class="content wide">
        <h1 class="page-title">Alpha instructions</h1>
        <p class="page-sub">
          The output of your alpha strategy: timestamped instructions the
          execution algorithm must work. Times are exchange-local; every
          instruction must trade #{symbol}, this run's symbol.
        </p>
        %{builder ~symbol ~side ~set_side ~qty ~set_qty ~arrival
            ~set_arrival ~deadline ~set_deadline ~update}
        <div class="row mt-md">
          <div class="grow">
            <textarea
              class="textarea"
              rows=%{14}
              %{Vdom.Attr.create "spellcheck" "false"}
              %{value_is alpha_text}
              %{on_input set_text}></textarea>
            <div class="row mt-md">
              <button class="btn" %{insert_sample}>Insert sample</button>
              <button class="btn ghost" %{clear}>Clear</button>
            </div>
          </div>
          <div class="grow">%{preview}</div>
        </div>
        <div class="row mt-xl">
          <button class="btn ghost" %{back}>← Back</button>
          <button class="btn primary" %{continue_} %{continue_disabled}>
            Continue to algorithm
          </button>
        </div>
      </div>
    |}
  ;;
end

(* ---------- Select the execution algorithm ---------- *)

module Algo_screen = struct
  let cards =
    [ ( Some Sim.Algo_choice.Twap
      , "TWAP"
      , "Time-weighted: split the order evenly across the window, \
         regardless of market activity. Predictable schedule; ignores where \
         the volume actually is." )
    ; ( None
      , "VWAP"
      , "Follow the historical intraday volume profile — trade more when \
         the market is typically busy (open, close). Promises when you \
         finish, not your share of volume." )
    ; ( None
      , "POV"
      , "Percent-of-volume: react to observed trading, holding a fixed \
         share of whatever actually prints. Promises market share, not a \
         finish time." )
    ; ( None
      , "Implementation shortfall"
      , "Dynamically trade off impact cost (from going fast) against drift \
         and opportunity cost (from going slow), using urgency, spread and \
         volatility." )
    ]
  ;;

  let component
    ~(model : Model.t Bonsai.t)
    ~(update : Model.updater Bonsai.t)
    (local_ _graph)
    =
    let%arr { Model.algo; _ } = model
    and update in
    let card_views =
      List.map cards ~f:(fun (choice, title, description) ->
        match choice with
        | Some this_algo ->
          let selected = Sim.Algo_choice.equal algo this_algo in
          let classes =
            Vdom.Attr.classes
              ([ "card-btn" ] @ if selected then [ "selected" ] else [])
          in
          let pick =
            on_click (update (fun m -> { m with Model.algo = this_algo }))
          in
          {%html|
            <button %{classes} %{pick}>
              <div class="card-title">#{title}
                <span class="badge">available</span></div>
              <div class="card-desc">#{description}</div>
            </button>
          |}
        | None ->
          {%html|
            <div class="card-btn disabled">
              <div class="card-title">#{title}
                <span class="badge soon">planned</span></div>
              <div class="card-desc">#{description}</div>
            </div>
          |})
    in
    let back = on_click (goto Model.Screen.Alpha update) in
    let continue_ = on_click (goto Model.Screen.Confirm update) in
    {%html|
      <div class="content">
        <h1 class="page-title">Execution algorithm</h1>
        <p class="page-sub">
          All algorithms answer the same question every minute — given the
          previous bar and the parent order's state, what child orders now? —
          so different choices are comparable on identical days.
        </p>
        <div class="algo-grid">*{card_views}</div>
        <div class="row mt-xl">
          <button class="btn ghost" %{back}>← Back</button>
          <button class="btn primary" %{continue_}>Review and run</button>
        </div>
      </div>
    |}
  ;;
end

(* ---------- Review and start ---------- *)

module Confirm = struct
  let field ~label ~help ~value ~set =
    {%html|
      <label class="field">
        <div class="field-label">#{label}</div>
        <input class="input num" %{value_is value} %{on_input set} />
        <div class="field-help">#{help}</div>
      </label>
    |}
  ;;

  let component
    ~(model : Model.t Bonsai.t)
    ~(update : Model.updater Bonsai.t)
    (local_ _graph)
    =
    let%arr model and update in
    let { Model.selection
        ; alpha_text
        ; algo
        ; half_spread_text
        ; participation_text
        ; impact_text
        ; run_error
        ; _
        }
      =
      model
    in
    let market =
      match selection with
      | None -> "—"
      | Some (symbol, date) ->
        [%string "%{Symbol.to_string symbol} on %{Fmt.date date}"]
    in
    let instructions_summary =
      match selection with
      | None -> "—"
      | Some (symbol, _) ->
        (match parse_alpha ~alpha_text ~symbol with
         | Error (_ : Error.t) -> "not parseable — go back to the alpha step"
         | Ok instructions ->
           let count side =
             List.count instructions ~f:(fun i -> Side.equal i.side side)
           in
           let total =
             List.sum (module Int) instructions ~f:(fun i ->
               Size.to_int i.quantity)
           in
           [%string
             "%{List.length instructions#Int} instructions (%{count \
              Side.Buy#Int} buys, %{count Side.Sell#Int} sells) · \
              %{Fmt.shares_int total} shares"])
    in
    let set field_update text = update (fun m -> field_update m text) in
    (* Validate the friction knobs as they are typed, not only at run time. *)
    let config_error =
      match fill_config_of_model model with
      | Ok (_ : Fill_model.Config.t) -> None
      | Error error -> Some error
    in
    let reset_defaults =
      on_click
        (update (fun m ->
           { m with
             Model.half_spread_text = Model.initial.Model.half_spread_text
           ; participation_text = Model.initial.Model.participation_text
           ; impact_text = Model.initial.Model.impact_text
           }))
    in
    let run_now =
      let%bind.Effect result = Effect.of_sync_fun run_from_model model in
      match result with
      | Error error ->
        update (fun m -> { m with Model.run_error = Some error })
      | Ok (output, record) ->
        update (fun m ->
          { m with
            Model.screen = Model.Screen.Simulate
          ; output = Some output
          ; run_error = None
          ; runs = record :: m.runs
          ; sim_minute = 0
          ; sim_playing = true
          })
    in
    let error_view =
      match run_error with
      | None -> Vdom.Node.none
      | Some error ->
        {%html|
          <div class="callout error mt-lg">
            The run failed:
            <pre>#{Error.to_string_hum error}</pre>
          </div>
        |}
    in
    let config_error_view =
      match config_error with
      | None -> Vdom.Node.none
      | Some error ->
        {%html|
          <div class="callout error mt-md">
            <pre>#{Error.to_string_hum error}</pre>
          </div>
        |}
    in
    {%html|
      <div class="content">
        <h1 class="page-title">Review and start</h1>
        <p class="page-sub">
          The run executes your instructions twice under identical market
          conditions — once with #{Sim.Algo_choice.display_name algo}, once
          with the naive immediate baseline — and grades both.
        </p>
        <div class="panel">
          <dl class="kv">
            <dt>Market day</dt><dd class="mono">#{market}</dd>
            <dt>Instructions</dt><dd>#{instructions_summary}</dd>
            <dt>Algorithm</dt><dd>#{Sim.Algo_choice.display_name algo}</dd>
            <dt>Baseline</dt><dd>Immediate full execution (market IOC each bar)</dd>
          </dl>
        </div>
        <div class="section-label">Market friction (Engine A fill model)</div>
        <div class="row">
          %{field ~label:"Half-spread ($)" ~value:half_spread_text
              ~help:"toll paid by liquidity-taking fills"
              ~set:(set (fun m text -> { m with Model.half_spread_text = text }))}
          %{field ~label:"Participation cap (0–1)" ~value:participation_text
              ~help:"max fraction of a bar's volume"
              ~set:(set (fun m text -> { m with Model.participation_text = text }))}
          %{field ~label:"Impact at 100% ($)" ~value:impact_text
              ~help:"square-root market impact coefficient"
              ~set:(set (fun m text -> { m with Model.impact_text = text }))}
          <button class="btn ghost self-end" %{reset_defaults}>Reset defaults</button>
        </div>
        %{config_error_view}
        <div class="row mt-xl">
          <button class="btn ghost" %{on_click (goto Model.Screen.Algo update)}>← Back</button>
          <button
            class="btn primary big"
            %{on_click run_now}
            %{disabled_if (Option.is_some config_error)}>Run simulation</button>
        </div>
        %{error_view}
      </div>
    |}
  ;;
end

(* ---------- Results ---------- *)

module Results = struct
  let metrics_cells (cost : Execlab_analytics.Transaction_cost.t) =
    match cost.fill_metrics with
    | None -> "—", "—", "—"
    | Some metrics ->
      ( Fmt.dollars_4dp metrics.average_fill_price
      , Fmt.bps metrics.shortfall_bps
      , Fmt.bps metrics.vwap_slippage_bps )
  ;;

  let graded_row index (graded : Sim.Graded.t) ~selected ~toggle =
    let cost = graded.algo in
    let avg_fill, shortfall, vwap_slip = metrics_cells cost in
    let value_tone =
      if graded.value_add_cents >= 0 then "num good" else "num bad"
    in
    let window =
      [%string
        "%{Fmt.ofday graded.instruction.arrival_time}→%{Fmt.ofday \
         graded.instruction.deadline}"]
    in
    let row_attrs =
      Vdom.Attr.many
        [ Vdom.Attr.classes
            ([ "clickable" ] @ if selected then [ "selected" ] else [])
        ; on_click toggle
        ]
    in
    {%html|
      <tr %{row_attrs}>
        <td class="num muted">#{Int.to_string (index + 1)}</td>
        <td>%{Side_badge.view cost.side}</td>
        <td class="num">#{Fmt.shares cost.quantity}</td>
        <td class="mono muted">#{window}</td>
        <td class="num">#{Fmt.price cost.arrival_price}</td>
        <td class="num">#{avg_fill}</td>
        <td class="num">#{shortfall}</td>
        <td class="num">#{vwap_slip}</td>
        <td class="num">#{Fmt.pct cost.completion_rate}</td>
        <td class="num">#{Fmt.signed_cents cost.net_pnl_cents}</td>
        <td class="num muted">#{Fmt.signed_cents graded.baseline.net_pnl_cents}</td>
        <td class=%{value_tone}>#{Fmt.signed_cents graded.value_add_cents}</td>
      </tr>
    |}
  ;;

  let day_leaderboard (runs : Model.Run_record.t list) ~symbol ~date =
    let matching =
      List.filter runs ~f:(fun run ->
        Symbol.equal run.symbol symbol && Date.equal run.date date)
    in
    match matching with
    | [] | [ _ ] -> Vdom.Node.none
    | _ :: _ :: _ ->
      let rows =
        List.map matching ~f:(fun run ->
          let capture =
            match run.alpha_capture with
            | None -> "—"
            | Some capture -> Fmt.pct capture
          in
          let tone =
            if run.value_add_cents >= 0 then "num good" else "num bad"
          in
          {%html|
            <tr>
              <td>#{run.algo_name}</td>
              <td class="num">#{capture}</td>
              <td class=%{tone}>#{Fmt.signed_cents run.value_add_cents}</td>
              <td class="num">#{Fmt.signed_cents run.net_cents}</td>
            </tr>
          |})
      in
      {%html|
        <div>
          <div class="section-label">Runs on this day (this browser session)</div>
          <div class="panel">
            <table class="table">
              <thead>
                <tr>
                  <th>Algo</th><th class="num">Captured</th>
                  <th class="num">Value add</th><th class="num">Net P&L</th>
                </tr>
              </thead>
              <tbody>*{rows}</tbody>
            </table>
          </div>
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
    let day =
      let%arr output in
      Option.map output ~f:(fun o -> o.Sim.Output.day)
    in
    (* Clicking a grading row isolates that instruction's fills on the chart;
       clicking it again shows everything. *)
    let focused, set_focused =
      Bonsai.state ~equal:[%equal: int option] None graph
    in
    let fills =
      let%arr output and focused in
      match output with
      | None -> []
      | Some output ->
        (match focused with
         | None -> output.fills
         | Some index ->
           Option.value (Map.find output.fills_by_parent index) ~default:[])
    in
    let chart = Chart.component ~day ~fills graph in
    let%arr output
    and chart
    and focused
    and set_focused
    and { Model.runs; _ } = model
    and update in
    match output with
    | None ->
      let start = on_click (goto Model.Screen.Choose_day update) in
      {%html|
        <div class="content">
          <h1 class="page-title">Results</h1>
          <div class="callout">No run yet.</div>
          <div class="mt-lg">
            <button class="btn primary" %{start}>Set one up</button>
          </div>
        </div>
      |}
    | Some output ->
      let totals = output.totals in
      let day = output.day in
      let capture_value =
        match Sim.Totals.alpha_capture totals with
        | None -> "n/a"
        | Some capture -> Fmt.pct capture
      in
      let tiles =
        [ Tile.view
            ~hero:true
            ~help:
              "Net P&L as a fraction of the alpha's theoretical P&L (every \
               instruction filled instantly and free at its arrival price). \
               The headline score."
            ~label:"Alpha captured"
            ~value:capture_value
            ~sub:
              ("net "
               ^ Fmt.signed_cents totals.net_cents
               ^ " / gross "
               ^ Fmt.signed_cents totals.gross_cents)
            ()
        ; Tile.view
            ~tone:totals.net_cents
            ~help:
              "What actually happened: filled shares valued at the session \
               close, minus what they really cost."
            ~label:"Net P&L"
            ~value:(Fmt.signed_cents totals.net_cents)
            ~sub:("gross theoretical " ^ Fmt.signed_cents totals.gross_cents)
            ()
        ; Tile.view
            ~tone:totals.value_add_cents
            ~help:
              "Algorithm net P&L minus the immediate-execution baseline's, \
               under identical market and fill-model conditions."
            ~label:"Execution value-add"
            ~value:(Fmt.signed_cents totals.value_add_cents)
            ~sub:
              ("immediate baseline net "
               ^ Fmt.signed_cents totals.baseline_net_cents)
            ()
        ; Tile.view
            ~help:
              "Shares executed before each instruction's deadline. Unfilled \
               shares of a correct alpha are pure opportunity cost."
            ~label:"Completion"
            ~value:(Fmt.pct (Sim.Totals.completion_rate totals))
            ~sub:
              (Fmt.shares_int totals.filled_shares
               ^ " of "
               ^ Fmt.shares_int totals.requested_shares
               ^ " shares")
            ()
        ]
      in
      let focus_note =
        match focused with
        | None -> ""
        | Some index -> [%string " — showing #%{index + 1#Int} only"]
      in
      let rows =
        List.mapi output.graded ~f:(fun index graded ->
          let selected = [%equal: int option] focused (Some index) in
          let toggle = set_focused (if selected then None else Some index) in
          graded_row index graded ~selected ~toggle)
      in
      let retest = on_click (goto Model.Screen.Algo update) in
      let new_sim = on_click (goto Model.Screen.Choose_day update) in
      let to_dashboard = on_click (goto Model.Screen.Dashboard update) in
      let replay =
        on_click
          (update (fun m ->
             { m with
               Model.screen = Model.Screen.Simulate
             ; sim_minute = 0
             ; sim_playing = true
             }))
      in
      let title =
        [%string
          "%{Symbol.to_string day.Trading_day.symbol} %{Fmt.date \
           day.Trading_day.date} · %{output.algo_name}"]
      in
      {%html|
        <div class="content wide">
          <h1 class="page-title">#{title}</h1>
          <p class="page-sub">
            Graded against the immediate-execution baseline under identical
            market and fill-model conditions ·
            #{Int.to_string (List.length output.fills)} fills ·
            day VWAP #{Fmt.dollars_4dp output.day_vwap}
          </p>
          <div class="row">*{tiles}</div>
          <div class="section-label">Session and fills#{focus_note}</div>
          %{chart}
          <div class="section-label">Per-instruction grading
            <span class="section-hint">click a row to isolate its fills on the chart</span></div>
          <div class="panel scroll-x">
            <table class="table">
              <thead>
                <tr>
                  <th class="num">#</th><th>Side</th><th class="num">Qty</th>
                  <th>Window</th>
                  <th class="num has-help"
                    %{help "Market price the minute the instruction activated — the decision-moment benchmark"}>Arrival</th>
                  <th class="num">Avg fill</th>
                  <th class="num has-help"
                    %{help "Implementation shortfall: avg fill vs arrival price, in basis points. Positive = worse than arrival."}>Shortfall bps</th>
                  <th class="num has-help"
                    %{help "Avg fill vs the day's volume-weighted average price. Positive = traded worse than the crowd."}>vs VWAP bps</th>
                  <th class="num">Filled</th>
                  <th class="num">Net P&L</th>
                  <th class="num has-help"
                    %{help "Net P&L of the naive baseline: demand everything immediately with market orders"}>Immediate</th>
                  <th class="num has-help"
                    %{help "Algorithm net P&L minus the immediate baseline's — positive means the algorithm earned its keep"}>Value add</th>
                </tr>
              </thead>
              <tbody>*{rows}</tbody>
            </table>
          </div>
          %{day_leaderboard runs
              ~symbol:day.Trading_day.symbol ~date:day.Trading_day.date}
          <div class="row mt-xl">
            <button class="btn primary" %{retest}>Retest with a different algorithm</button>
            <button class="btn" %{replay}>Replay the session</button>
            <button class="btn" %{new_sim}>New simulation</button>
            <button class="btn ghost" %{to_dashboard}>Dashboard</button>
          </div>
        </div>
      |}
  ;;
end
