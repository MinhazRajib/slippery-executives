open! Core
open! Execlab_types

let dollar_string cents =
  let magnitude = Price.to_string_dollar (Price.of_int_cents (abs cents)) in
  if cents < 0 then "-" ^ magnitude else magnitude
;;

let to_string_hum (tc : Transaction_cost.t) =
  let header =
    [%string
      "%{tc.symbol#Symbol} %{tc.side#Side} %{tc.quantity#Size} shares"]
  in
  let overview =
    [ sprintf
        "  filled          %d of %d (%.1f%%)"
        (Size.to_int tc.filled)
        (Size.to_int tc.quantity)
        (tc.completion_rate *. 100.)
    ; [%string
        "  arrival         %{Price.to_string_dollar tc.arrival_price}"]
    ; [%string
        "  terminal        %{Price.to_string_dollar tc.terminal_price}"]
    ; sprintf "  day vwap        $%.4f" tc.day_vwap
    ]
  in
  let fill_story =
    match tc.fill_metrics with
    | None -> [ "  no fills" ]
    | Some { average_fill_price; shortfall_bps; vwap_slippage_bps } ->
      [ sprintf "  avg fill        $%.4f" average_fill_price
      ; sprintf
          "  shortfall       %+.1f bps (%s)"
          shortfall_bps
          (dollar_string tc.friction_cost_cents)
      ; sprintf "  vs day vwap     %+.1f bps" vwap_slippage_bps
      ; [%string "  spread cost     %{dollar_string tc.spread_cost_cents}"]
      ]
  in
  let pnl_story =
    [ [%string
        "  gross alpha     %{dollar_string tc.gross_theoretical_pnl_cents}"]
    ; [%string "  net P&L         %{dollar_string tc.net_pnl_cents}"]
    ; [%string
        "  opportunity     %{dollar_string tc.opportunity_cost_cents}"]
    ; (match tc.alpha_capture with
       | None -> "  alpha captured  n/a (gross alpha not positive)"
       | Some capture -> sprintf "  alpha captured  %.1f%%" (capture *. 100.))
    ]
  in
  String.concat ~sep:"\n" ((header :: overview) @ fill_story @ pnl_story)
;;

let comparison ~algo ~algo_name ~baseline ~baseline_name =
  let open Or_error.Let_syntax in
  let%map value_add_cents =
    Transaction_cost.value_add_cents ~algo ~baseline
  in
  String.concat
    ~sep:"\n"
    [ [%string "=== %{algo_name} ==="]
    ; to_string_hum algo
    ; [%string "=== %{baseline_name} ==="]
    ; to_string_hum baseline
    ; [%string
        "value added (%{algo_name} - %{baseline_name}): %{dollar_string \
         value_add_cents}"]
    ]
;;
