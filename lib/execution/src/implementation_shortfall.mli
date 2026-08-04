(** Implementation shortfall: trade the impact cost of hurrying against the
    drift risk of waiting. {!Twap} spreads evenly and {!Vwap} follows the
    tape; this algorithm {e front-loads} — do more while the remaining
    position (and so the exposure to adverse drift) is large, ease off as it
    shrinks.

    The schedule is the Almgren-Chriss optimal-liquidation trajectory. At
    elapsed fraction [f] of the arrival -> deadline window, the scheduled
    {e remaining} quantity is

    {v remaining(f) = total * sinh(urgency * (1 - f)) / sinh(urgency) v}

    and the target is [total - remaining], rounded to nearest. In the theory
    the curvature is [kappa = sqrt(risk_aversion * variance / impact) * T];
    rather than estimate those pieces separately we expose their product as
    the single dimensionless [urgency] knob:

    - [urgency = 0] {e is} {!Twap}, computed by Twap's own arithmetic so the
      two agree share for share;
    - large [urgency] approaches {!Immediate} (risk swamps impact);
    - around [2.] front-loads noticeably without slamming the tape.

    At [f = 0] the target is exactly [0]; at [f = 1] it is exactly [total]
    ([sinh 0 = 0]), so the deadline never strands shares. Everything below
    the schedule is {!Twap}'s machinery: the deficit against
    filled-plus-working, market IOC children, participation-cap remainders
    re-demanded the next bar. *)

open! Core
open! Execlab_types

(** [create ~urgency ()] closes over the knob because {!Algorithm_intf.S}
    gives [init] no configuration of its own. [urgency <= 0] is the TWAP
    limit; the unit argument leaves room for the planned knobs (price
    sensitivity, max participation). *)
val create : urgency:float -> unit -> Algorithm_intf.t
