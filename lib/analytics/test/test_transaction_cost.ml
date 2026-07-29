open! Core
open! Execlab_types
open Execlab_analytics

let instruction ?(quantity = 100) side =
  Or_error.ok_exn
    (Alpha_instruction.create
       ~arrival_time:(Time_ns.Ofday.of_string "10:00:00")
       ~symbol:(Symbol.of_string "NVDA")
       ~side
       ~quantity:(Size.of_int quantity)
       ~deadline:(Time_ns.Ofday.of_string "11:00:00"))
;;

let fill
  ?(symbol = "NVDA")
  ?(liquidity = Liquidity.Taker)
  side
  size
  price_cents
  =
  { Fill.fill_id = 0
  ; symbol = Symbol.of_string symbol
  ; price = Price.of_int_cents price_cents
  ; size = Size.of_int size
  ; order_id = Order_id.For_testing.of_int 1
  ; side
  ; time = Time_ns.Ofday.of_string "10:15:00"
  ; liquidity
  }
;;

let grade
  ?(arrival = 1000)
  ?(terminal = 1100)
  ?(day_vwap = 10.15)
  ?(half_spread = 1)
  instruction
  fills
  =
  Transaction_cost.create
    ~instruction
    ~fills
    ~arrival_price:(Price.of_int_cents arrival)
    ~terminal_price:(Price.of_int_cents terminal)
    ~day_vwap
    ~half_spread:(Price.of_int_cents half_spread)
;;

let identity_holds (tc : Transaction_cost.t) =
  tc.gross_theoretical_pnl_cents
  = tc.net_pnl_cents + tc.friction_cost_cents + tc.opportunity_cost_cents
;;

let%expect_test "full fill, buy: every number by hand" =
  (* Buy 100 arriving at $10.00; fills average $10.20; the stock ends the day
     at $11.00. Gross alpha $100; execution friction $20; net $80. *)
  let tc =
    Or_error.ok_exn
      (grade (instruction Buy) [ fill Buy 50 1010; fill Buy 50 1030 ])
  in
  print_s [%sexp (tc : Transaction_cost.t)];
  [%expect
    {|
    ((symbol NVDA) (side Buy) (quantity 100) (filled 100) (completion_rate 1)
     (arrival_price 1000) (terminal_price 1100) (day_vwap 10.15)
     (fill_metrics
      (((average_fill_price 10.2) (shortfall_bps 199.99999999999929)
        (vwap_slippage_bps 49.261083743841318))))
     (spread_cost_cents 100) (friction_cost_cents 2000)
     (opportunity_cost_cents 0) (gross_theoretical_pnl_cents 10000)
     (net_pnl_cents 8000) (alpha_capture (0.8)))
    |}];
  printf "identity holds: %b\n" (identity_holds tc);
  [%expect {| identity holds: true |}]
;;

let%expect_test "sell mirror: same magnitudes, positive still means worse" =
  (* Sell 100 arriving at $10.00; fills average $9.80 (received less =
     friction); the stock ends at $9.00, so the sell alpha was right. *)
  let tc =
    Or_error.ok_exn
      (grade
         ~terminal:900
         ~day_vwap:9.85
         (instruction Sell)
         [ fill Sell 50 990; fill Sell 50 970 ])
  in
  let { Transaction_cost.Fill_metrics.average_fill_price
      ; shortfall_bps
      ; vwap_slippage_bps
      }
    =
    Option.value_exn tc.fill_metrics
  in
  printf "avg fill: %.2f\n" average_fill_price;
  printf "shortfall: %+.1f bps\n" shortfall_bps;
  printf "vs vwap: %+.1f bps\n" vwap_slippage_bps;
  printf
    "gross: %d  net: %d  friction: %d  opportunity: %d\n"
    tc.gross_theoretical_pnl_cents
    tc.net_pnl_cents
    tc.friction_cost_cents
    tc.opportunity_cost_cents;
  printf "identity holds: %b\n" (identity_holds tc);
  [%expect
    {|
    avg fill: 9.80
    shortfall: +200.0 bps
    vs vwap: +50.8 bps
    gross: 10000  net: 8000  friction: 2000  opportunity: 0
    identity holds: true
    |}]
;;

let%expect_test "partial fill: the unfilled remainder is opportunity cost" =
  (* Buy 100, only 40 fill at $10.10; the stock ends at $11.00. The 60
     unfilled shares of a correct alpha are pure lost alpha: $60. *)
  let tc = Or_error.ok_exn (grade (instruction Buy) [ fill Buy 40 1010 ]) in
  printf "completion: %.0f%%\n" (tc.completion_rate *. 100.);
  printf
    "gross: %d  net: %d  friction: %d  opportunity: %d\n"
    tc.gross_theoretical_pnl_cents
    tc.net_pnl_cents
    tc.friction_cost_cents
    tc.opportunity_cost_cents;
  printf "identity holds: %b\n" (identity_holds tc);
  printf
    "alpha capture: %s\n"
    (match tc.alpha_capture with
     | None -> "n/a"
     | Some capture -> sprintf "%.2f" capture);
  [%expect
    {|
    completion: 40%
    gross: 10000  net: 3600  friction: 400  opportunity: 6000
    identity holds: true
    alpha capture: 0.36
    |}]
;;

let%expect_test "no fills at all is a legitimate grading: all opportunity" =
  let tc = Or_error.ok_exn (grade (instruction Buy) []) in
  printf "fill metrics: %b\n" (Option.is_some tc.fill_metrics);
  printf
    "gross: %d  net: %d  opportunity: %d\n"
    tc.gross_theoretical_pnl_cents
    tc.net_pnl_cents
    tc.opportunity_cost_cents;
  printf "identity holds: %b\n" (identity_holds tc);
  [%expect
    {|
    fill metrics: false
    gross: 10000  net: 0  opportunity: 10000
    identity holds: true
    |}]
;;

let%expect_test "wrong-way alpha: negative gross, alpha capture undefined" =
  (* Buy 100 but the stock falls to $9.00: no positive alpha to capture, and
     the unfilled shares were a lucky save (negative opportunity). *)
  let tc =
    Or_error.ok_exn
      (grade ~terminal:900 (instruction Buy) [ fill Buy 40 1010 ])
  in
  printf
    "gross: %d  net: %d  opportunity: %d\n"
    tc.gross_theoretical_pnl_cents
    tc.net_pnl_cents
    tc.opportunity_cost_cents;
  printf "identity holds: %b\n" (identity_holds tc);
  printf "alpha capture: %b\n" (Option.is_some tc.alpha_capture);
  [%expect
    {|
    gross: -10000  net: -4400  opportunity: -6000
    identity holds: true
    alpha capture: false
    |}]
;;

let%expect_test "spread cost: taker fills pay the toll, maker fills earn it \
                 back"
  =
  let graded fills =
    (Or_error.ok_exn (grade ~half_spread:2 (instruction Buy) fills))
      .spread_cost_cents
  in
  printf "all taker: %d\n" (graded [ fill Buy 50 1010; fill Buy 50 1010 ]);
  printf
    "half maker: %d\n"
    (graded
       [ fill Buy 50 1010; fill ~liquidity:Liquidity.Maker Buy 50 1010 ]);
  [%expect {|
    all taker: 200
    half maker: 0
    |}]
;;

let%expect_test "fills that do not belong to the instruction are errors" =
  let print_error result =
    print_s [%sexp (Or_error.ignore_m result : unit Or_error.t)]
  in
  print_error (grade (instruction Buy) [ fill Sell 10 1010 ]);
  [%expect
    {|
    (Error
     ("Transaction_cost.create: fill does not belong to the instruction"
      (fill
       ((fill_id 0) (symbol NVDA) (price 1010) (size 10) (order_id 1) (side Sell)
        (time 10:15:00.000000000) (liquidity Taker)))
      (instruction
       ((arrival_time 10:00:00.000000000) (symbol NVDA) (side Buy) (quantity 100)
        (deadline 11:00:00.000000000)))))
    |}];
  print_error (grade (instruction Buy) [ fill ~symbol:"TSLA" Buy 10 1010 ]);
  [%expect
    {|
    (Error
     ("Transaction_cost.create: fill does not belong to the instruction"
      (fill
       ((fill_id 0) (symbol TSLA) (price 1010) (size 10) (order_id 1) (side Buy)
        (time 10:15:00.000000000) (liquidity Taker)))
      (instruction
       ((arrival_time 10:00:00.000000000) (symbol NVDA) (side Buy) (quantity 100)
        (deadline 11:00:00.000000000)))))
    |}];
  print_error (grade (instruction ~quantity:5 Buy) [ fill Buy 10 1010 ]);
  [%expect
    {|
    (Error
     ("Transaction_cost.create: fills exceed the instruction's quantity"
      (filled 10) (quantity 5)))
    |}]
;;

let%expect_test "value add: same instruction compares, different does not" =
  let algo =
    Or_error.ok_exn (grade (instruction Buy) [ fill Buy 100 1010 ])
  in
  let baseline =
    Or_error.ok_exn (grade (instruction Buy) [ fill Buy 100 1030 ])
  in
  print_s
    [%sexp
      (Transaction_cost.value_add_cents ~algo ~baseline : int Or_error.t)];
  [%expect {| (Ok 2000) |}];
  let other =
    Or_error.ok_exn
      (grade (instruction ~quantity:50 Buy) [ fill Buy 50 1010 ])
  in
  printf
    "different quantity comparable: %b\n"
    (Or_error.is_ok (Transaction_cost.value_add_cents ~algo ~baseline:other));
  [%expect {| different quantity comparable: false |}]
;;
