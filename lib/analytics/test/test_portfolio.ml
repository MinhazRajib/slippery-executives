open! Core
open Execlab_types
open Execlab_analytics

let fill ?(symbol = "NVDA") side size price_cents =
  { Fill.fill_id = 0
  ; symbol = Symbol.of_string symbol
  ; price = Price.of_int_cents price_cents
  ; size = Size.of_int size
  ; order_id = Order_id.For_testing.of_int 1
  ; side
  ; time = Time_ns.Ofday.of_string "10:00:00"
  ; liquidity = Liquidity.Taker
  }
;;

let%expect_test "buys blend the average cost; sells realize against it" =
  let t = Portfolio.create ~starting_cash_cents:10_000_000 in
  Portfolio.apply_fill t (fill Buy 100 1000);
  print_endline (Portfolio.to_string t);
  [%expect {| cash $99000.00, realized $0.00; NVDA +100 @ $10.00 |}];
  Portfolio.apply_fill t (fill Buy 100 1200);
  print_endline (Portfolio.to_string t);
  [%expect {| cash $97800.00, realized $0.00; NVDA +200 @ $11.00 |}];
  Portfolio.apply_fill t (fill Sell 50 1300);
  print_endline (Portfolio.to_string t);
  [%expect {| cash $98450.00, realized $100.00; NVDA +150 @ $11.00 |}];
  let symbol = Symbol.of_string "NVDA" in
  let mark = Price.of_int_cents 1250 in
  printf
    "unrealized at $12.50: %d\n"
    (Portfolio.unrealized_pnl_cents t symbol ~mark);
  let equity = Portfolio.equity_cents t ~mark:(fun (_ : Symbol.t) -> mark) in
  printf "equity: %d\n" equity;
  printf
    "identity holds: %b\n"
    (equity - Portfolio.starting_cash_cents t
     = Portfolio.realized_pnl_cents t
       + Portfolio.unrealized_pnl_cents t symbol ~mark);
  [%expect
    {|
    unrealized at $12.50: 22500
    equity: 10032500
    identity holds: true
    |}]
;;

let%expect_test "a fill through flat closes the long, then opens a short at \
                 the fill price"
  =
  let t = Portfolio.create ~starting_cash_cents:10_000_000 in
  Portfolio.apply_fill t (fill Buy 100 1100);
  Portfolio.apply_fill t (fill Sell 250 1300);
  print_endline (Portfolio.to_string t);
  [%expect {| cash $102150.00, realized $200.00; NVDA -150 @ $13.00 |}];
  Portfolio.apply_fill t (fill Buy 150 1200);
  print_endline (Portfolio.to_string t);
  [%expect {| cash $100350.00, realized $350.00; no positions |}];
  printf
    "equity - starting = %d\n"
    (Portfolio.equity_cents t ~mark:(fun (_ : Symbol.t) ->
       Price.of_int_cents 9999)
     - Portfolio.starting_cash_cents t);
  [%expect {| equity - starting = 35000 |}]
;;

let%expect_test "partial-close rounding never creates or destroys a cent" =
  let t = Portfolio.create ~starting_cash_cents:0 in
  (* Basis of 4003 cents over 4 shares: 1000.75 cents per share, so a
     one-share close must round. *)
  Portfolio.apply_fill t (fill Buy 1 1000);
  Portfolio.apply_fill t (fill Buy 3 1001);
  Portfolio.apply_fill t (fill Sell 1 1100);
  let symbol = Symbol.of_string "NVDA" in
  printf "realized after 1: %d\n" (Portfolio.realized_pnl_cents t);
  printf "basis left: %d\n" (Portfolio.cost_basis_cents t symbol);
  Portfolio.apply_fill t (fill Sell 3 1100);
  printf "realized after all: %d\n" (Portfolio.realized_pnl_cents t);
  printf "cash: %d\n" (Portfolio.cash_cents t);
  [%expect
    {|
    realized after 1: 99
    basis left: 3002
    realized after all: 397
    cash: 397
    |}]
;;

let%expect_test "marking a short: profits when the price falls" =
  let t = Portfolio.create ~starting_cash_cents:10_000_000 in
  Portfolio.apply_fill t (fill Sell 100 1500);
  let symbol = Symbol.of_string "NVDA" in
  printf "position: %d\n" (Portfolio.position t symbol);
  printf
    "unrealized at $14.00: %d\n"
    (Portfolio.unrealized_pnl_cents t symbol ~mark:(Price.of_int_cents 1400));
  printf
    "unrealized at $16.00: %d\n"
    (Portfolio.unrealized_pnl_cents t symbol ~mark:(Price.of_int_cents 1600));
  [%expect
    {|
    position: -100
    unrealized at $14.00: 10000
    unrealized at $16.00: -10000
    |}]
;;

let%expect_test "a zero-size fill is a fill-engine bug and raises" =
  let t = Portfolio.create ~starting_cash_cents:0 in
  Expect_test_helpers_core.show_raise (fun () ->
    Portfolio.apply_fill t (fill Buy 0 1000));
  [%expect
    {|
    (raised (
      "Portfolio.apply_fill: fill size must be positive"
      (fill (
        (fill_id   0)
        (symbol    NVDA)
        (price     1000)
        (size      0)
        (order_id  1)
        (side      Buy)
        (time      10:00:00.000000000)
        (liquidity Taker)))))
    |}]
;;
