(** Scoring layer of the platform: turns the stream of {!Fill}s a run
    produces into the numbers users are judged on.

    The modules mirror the tail of the pipeline in [context.md]: {!Portfolio}
    accounts for fills (cash, positions, realized and unrealized P&L);
    {!Benchmarks} extracts the reference prices — arrival and terminal — from
    the historical session (day VWAP comes from {!Day_stats});
    {!Transaction_cost} grades one instruction's fills against those
    benchmarks (shortfall, VWAP slippage, spread cost, opportunity cost,
    alpha capture, and the accounting identity tying them together);
    {!Report} renders gradings for the terminal. *)

module Benchmarks = Benchmarks
module Portfolio = Portfolio
module Report = Report
module Transaction_cost = Transaction_cost
