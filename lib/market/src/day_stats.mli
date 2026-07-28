(** Statistics derived from one {!Trading_day}: the numbers the VWAP
    algorithm, the fill model, and the transaction-cost analytics consume.

    Prices you can trade at are {!Price.t}; statistics about prices are
    [float] dollars. *)

open! Core
open! Execlab_types

(** Total shares traded across the session. *)
val total_volume : Trading_day.t -> Size.t

(** Volume-weighted average price for the day, in dollars. Each bar's price
    is approximated by its typical price [(high + low + close) / 3]. Raises
    if the day's total volume is zero. *)
val vwap : Trading_day.t -> float

(** Each bar's fraction of the day's volume, in bar order; sums to 1. The
    historical schedule the VWAP execution algorithm follows. Raises if the
    day's total volume is zero. *)
val volume_profile : Trading_day.t -> float list

(** Daily realized volatility: the sample standard deviation of
    close-to-close minute log returns, scaled by [sqrt 390] (minutes per
    session). *)
val realized_volatility : Trading_day.t -> float
