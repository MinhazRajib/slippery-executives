(** Scoring layer of the platform: turns the stream of {!Fill}s a run
    produces into the numbers users are judged on.

    {!Portfolio} is the accounting foundation (cash, positions,
    realized/unrealized P&L); benchmarks and transaction-cost metrics build
    on it next. Re-export each new module here so the whole library is
    reachable through the top-level {!Execlab_analytics} module. *)

module Portfolio = Portfolio
