(** POV (percent-of-volume): no schedule at all — chase the {e realized}
    tape, holding cumulative demand at a fixed share of the volume actually
    observed since arrival. Where {!Twap} follows the clock and the planned
    VWAP follows the {e forecast} tape, POV follows the market as it happens:
    it promises your share of real-time volume, not a finish time, so the two
    families fail differently on abnormal days.

    The demand target at each bar is

    {v floor (participation_rate * observed_volume) v}

    where [observed_volume] sums the volume of every completed bar whose time
    is at-or-after the instruction's arrival (the bar preceding activation is
    pre-decision history and never counts). The target is floored, never
    rounded, because the rate is a {e ceiling}: rounding up could demand more
    than the configured share of the tape. Whatever part of the target is
    neither filled nor working is submitted as a market IOC child, so
    shortfalls from participation-capped fills are re-demanded the next
    minute, exactly like {!Twap}'s catch-up loop.

    Because a pure POV never promises completion but our parents expire at
    the deadline, the deadline bar (the last one that trades) demands the
    whole remainder regardless of rate — mirroring {!Twap}'s target reaching
    [total] at the deadline, and keeping completion rates comparable across
    algorithms. This catch-up ignores [min_child_size] and [max_child_size].

    Note the tape here is the historical bar volume, which does not contain
    our own fills — [participation_rate] is a share of the {e historical}
    session, not of (history + us). A rolling volume window for smoothing is
    a planned knob, not yet implemented. *)

open! Core
open! Execlab_types

(** [create ~participation_rate] closes over the configuration because
    {!Algorithm_intf.S} gives [init] no configuration of its own.
    [participation_rate] is a fraction in (0, 1\] — [0.05] demands 5% of
    observed volume. [min_child_size] holds back slices smaller than this
    (dust waits until the target accumulates); [max_child_size] caps any
    single slice:

    {[
      Driver.run
        ~algorithm:(Pov.create ~participation_rate:0.1 ())
        ~day
        ~instructions
        ()
    ]} *)
val create
  :  ?min_child_size:Size.t
  -> ?max_child_size:Size.t
  -> participation_rate:float
  -> unit
  -> Algorithm_intf.t
