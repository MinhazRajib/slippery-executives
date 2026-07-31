(** VWAP: {!Twap}'s catch-up loop with the schedule shaped by a historical
    intraday volume profile instead of the clock — trade more when the market
    is typically busy (the open, the close), less through the quiet middle.

    VWAP promises {e when} you finish (the deadline), not what share of
    real-time volume you take: it follows the forecast tape, where the
    planned POV algorithm will follow the realized one, so the two fail
    differently on abnormal days.

    The schedule target at time [now] is

    {v total * weight [arrival, now) / weight [arrival, deadline) v}

    rounded to nearest, where [weight] sums the profile entries whose times
    fall in the half-open interval. Entries at the deadline itself are
    excluded, so the target reaches exactly [total] at the deadline — the
    same convention as {!Twap}'s [total * m / n]. Everything past the target
    that is neither filled nor working is submitted as a market IOC child, so
    shortfalls from participation-capped fills are re-demanded the next
    minute. *)

open! Core
open! Execlab_types

(** [create ~profile] closes over the profile because {!Algorithm_intf.S}
    gives [init] no configuration of its own. [profile] pairs each bar's
    time with that bar's fraction of the day's volume — obtained by zipping
    [Day_stats.volume_profile] with the day's bar times:

    {[
      let profile =
        List.map2_exn
          day.Trading_day.bars
          (Day_stats.volume_profile day)
          ~f:(fun bar weight -> bar.Market_bar.time, weight)
      in
      Driver.run ~algorithm:(Vwap.create ~profile) ...
    ]}

    Weights must be non-negative; entries outside [arrival, deadline) are
    ignored. If no weight falls inside the window (deadline = arrival, or
    an empty profile), the whole quantity is due immediately, like {!Twap}
    with a zero-length window. *)
val create : profile:(Time_ns.Ofday.t * float) list -> Algorithm_intf.t
