open! Core
open! Execlab_types
open! Execlab_market

module Fill_metrics = struct
  type t =
    { average_fill_price : float
    ; shortfall_bps : float
    ; vwap_slippage_bps : float
    }
  [@@deriving sexp_of]
end

type t =
  { symbol : Symbol.t
  ; side : Side.t
  ; quantity : Size.t
  ; filled : Size.t
  ; completion_rate : float
  ; arrival_price : Price.t
  ; terminal_price : Price.t
  ; day_vwap : float
  ; fill_metrics : Fill_metrics.t option
  ; timing_cost_cents : int
  ; spread_cost_cents : int
  ; impact_cost_cents : int
  ; friction_cost_cents : int
  ; opportunity_cost_cents : int
  ; gross_theoretical_pnl_cents : int
  ; net_pnl_cents : int
  ; alpha_capture : float option
  }
[@@deriving sexp_of]

let bps_vs_benchmark ~side_sign ~average_fill_price ~benchmark =
  Float.of_int side_sign
  *. ((average_fill_price -. benchmark) /. benchmark)
  *. 10_000.
;;

(* Splits one fill's cost-vs-arrival into the metric tree. The fill model
   prices a taking fill at [bar open + sign * (half_spread + impact)], so
   arrival -> open is the market's drift (timing), the half-spread is the
   crossing toll, and whatever remains beyond [open + half_spread] is our own
   impact (negative only when a crossing limit clamped the price). A
   providing fill trades at its resting limit: no toll, no impact — the whole
   distance from arrival is timing. *)
let decompose_fill
  ~open_by_time
  ~side_sign
  ~arrival_cents
  ~half_spread
  (fill : Fill.t)
  =
  let size = Size.to_int fill.size in
  let price_cents = Price.to_int_cents fill.price in
  match fill.liquidity with
  | Liquidity.Maker ->
    Ok (`Timing (side_sign * size * (price_cents - arrival_cents)))
  | Taker ->
    (match Map.find open_by_time fill.time with
     | None ->
       Or_error.error_s
         [%message
           "Transaction_cost.create: no bar at a fill's time" (fill : Fill.t)]
     | Some bar_open ->
       let open_cents = Price.to_int_cents bar_open in
       let timing = side_sign * size * (open_cents - arrival_cents) in
       let spread = size * Price.to_int_cents half_spread in
       let impact =
         (side_sign * size * (price_cents - open_cents)) - spread
       in
       Ok (`Taker (timing, spread, impact)))
;;

let create
  ~(instruction : Alpha_instruction.t)
  ~fills
  ~(day : Trading_day.t)
  ~arrival_price
  ~terminal_price
  ~day_vwap
  ~half_spread
  =
  let foreign_fill =
    List.find fills ~f:(fun (fill : Fill.t) ->
      (not (Symbol.equal fill.symbol instruction.symbol))
      || not (Side.equal fill.side instruction.side))
  in
  match foreign_fill with
  | Some fill ->
    Or_error.error_s
      [%message
        "Transaction_cost.create: fill does not belong to the instruction"
          (fill : Fill.t)
          (instruction : Alpha_instruction.t)]
  | None ->
    let filled =
      List.fold fills ~init:Size.zero ~f:(fun acc (fill : Fill.t) ->
        Size.( + ) acc fill.size)
    in
    if Size.( > ) filled instruction.quantity
    then
      Or_error.error_s
        [%message
          "Transaction_cost.create: fills exceed the instruction's quantity"
            (filled : Size.t)
            ~quantity:(instruction.quantity : Size.t)]
    else (
      let side_sign = Side.sign instruction.side in
      let arrival_cents = Price.to_int_cents arrival_price in
      let terminal_cents = Price.to_int_cents terminal_price in
      let filled_shares = Size.to_int filled in
      let quantity_shares = Size.to_int instruction.quantity in
      let notional_cents =
        List.sum (module Int) fills ~f:Fill.notional_cents
      in
      (* Every identity term telescopes around the arrival price:
         - gross = sign * quantity * (terminal - arrival)
         - net = sign * (filled * terminal - notional)
         - friction = sign * (notional - filled * arrival)
         - opportunity = sign * (quantity - filled) * (terminal - arrival) so
           gross = net + friction + opportunity holds exactly, in ints. *)
      let gross_theoretical_pnl_cents =
        side_sign * quantity_shares * (terminal_cents - arrival_cents)
      in
      let net_pnl_cents =
        side_sign * ((filled_shares * terminal_cents) - notional_cents)
      in
      let friction_cost_cents =
        side_sign * (notional_cents - (filled_shares * arrival_cents))
      in
      let opportunity_cost_cents =
        side_sign
        * (quantity_shares - filled_shares)
        * (terminal_cents - arrival_cents)
      in
      let open_by_time =
        Map.of_alist_exn
          (module Time_ns.Ofday)
          (List.map day.bars ~f:(fun bar -> bar.Market_bar.time, bar.open_))
      in
      let decomposition =
        List.map
          fills
          ~f:
            (decompose_fill
               ~open_by_time
               ~side_sign
               ~arrival_cents
               ~half_spread)
        |> Or_error.combine_errors
      in
      match decomposition with
      | Error error -> Error error
      | Ok pieces ->
        let timing_cost_cents, spread_cost_cents, impact_cost_cents =
          List.fold
            pieces
            ~init:(0, 0, 0)
            ~f:(fun (timing, spread, impact) piece ->
              match piece with
              | `Timing t -> timing + t, spread, impact
              | `Taker (t, s, i) -> timing + t, spread + s, impact + i)
        in
        let fill_metrics =
          if filled_shares = 0
          then None
          else (
            let average_fill_price =
              Float.of_int notional_cents
              /. 100.
              /. Float.of_int filled_shares
            in
            Some
              { Fill_metrics.average_fill_price
              ; shortfall_bps =
                  bps_vs_benchmark
                    ~side_sign
                    ~average_fill_price
                    ~benchmark:(Price.to_float arrival_price)
              ; vwap_slippage_bps =
                  bps_vs_benchmark
                    ~side_sign
                    ~average_fill_price
                    ~benchmark:day_vwap
              })
        in
        let alpha_capture =
          if gross_theoretical_pnl_cents > 0
          then
            Some
              (Float.of_int net_pnl_cents
               /. Float.of_int gross_theoretical_pnl_cents)
          else None
        in
        Ok
          { symbol = instruction.symbol
          ; side = instruction.side
          ; quantity = instruction.quantity
          ; filled
          ; completion_rate =
              Float.of_int filled_shares /. Float.of_int quantity_shares
          ; arrival_price
          ; terminal_price
          ; day_vwap
          ; fill_metrics
          ; timing_cost_cents
          ; spread_cost_cents
          ; impact_cost_cents
          ; friction_cost_cents
          ; opportunity_cost_cents
          ; gross_theoretical_pnl_cents
          ; net_pnl_cents
          ; alpha_capture
          })
;;

let value_add_cents ~algo ~baseline =
  let same_grading =
    Symbol.equal algo.symbol baseline.symbol
    && Side.equal algo.side baseline.side
    && Size.equal algo.quantity baseline.quantity
    && Price.equal algo.arrival_price baseline.arrival_price
    && Price.equal algo.terminal_price baseline.terminal_price
  in
  if same_grading
  then Ok (algo.net_pnl_cents - baseline.net_pnl_cents)
  else
    Or_error.error_s
      [%message
        "Transaction_cost.value_add_cents: gradings are of different \
         instructions"
          (algo : t)
          (baseline : t)]
;;
