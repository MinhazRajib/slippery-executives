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
    gives [init] no configuration of its own. [profile] pairs each bar's time
    with that minute's forecast fraction of the day's volume. The honest
    forecast is [Day_stats.average_volume_profile] over *other* sessions of
    the same symbol — never the simulated day itself, which would let the
    algorithm peek at the volume it is about to trade:

    {[
      let profile =
        List.map2_exn
          day.Trading_day.bars
          (Or_error.ok_exn (Day_stats.average_volume_profile other_days))
          ~f:(fun bar weight -> bar.Market_bar.time, weight)
      in
      Driver.run ~algorithm:(Vwap.create ~profile) ...
    ]}

    Weights must be non-negative; entries outside the arrival -> deadline
    window are ignored. If no weight falls inside the window (deadline =
    arrival, or an empty profile), the whole quantity is due immediately,
    like {!Twap} with a zero-length window. *)
val create : profile:(Time_ns.Ofday.t * float) list -> Algorithm_intf.t
