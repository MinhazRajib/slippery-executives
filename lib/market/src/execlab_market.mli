(** Historical market data for the execution laboratory.

    {!Market_bar} is one minute of OHLCV history; the data loader (coming
    next) reads the bundled files under [data/] into validated bars.
    Re-export each new module here so the whole library is reachable through
    the top-level {!Execlab_market} module. *)

module Data_loader = Data_loader
module Day_stats = Day_stats
module Market_bar = Market_bar
module Trading_day = Trading_day
