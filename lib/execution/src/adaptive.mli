(** Adaptive: the first algorithm here that decides {e how} to trade, not
    only how much.

    {!Twap}, {!Vwap}, {!Pov} and {!Implementation_shortfall} all answer one
    question — what quantity is owed this minute — and then answer it the
    same way every time, with a market order that crosses the spread. That
    makes the spread a fixed toll: every share of every run pays it. A desk
    does not trade that way. It rests an order when it is comfortable and
    crosses when it is not, and the choice between the two is most of what an
    execution algorithm is for.

    This algorithm keeps {!Twap}'s trajectory as the thing it is accountable
    to, and makes a style decision against it each bar:

    - {b behind the schedule by more than the drift budget} — cross for what
      the schedule is owed. Falling behind is how a run misses its deadline,
      and no price is worth that.
    - {b on schedule, and the tape is better than the decision price} — cross
      anyway. A price in front of the benchmark is the one worth paying the
      spread for, and it may not last.
    - {b on schedule, and the tape is not} — rest a limit order at the near
      touch and let the market come. A fill there is better than crossing for
      it at that moment by exactly the spread, whatever the price does next.

    The drift budget is what [patience] buys. At the start of the window it
    is [patience * 0.25] of the parent, and two things shrink it. It tapers
    with the time left, so patience can delay a fill but never cost a
    completion. And it is damped by how fast the market is moving away — the
    last bar's move against the order, halving the budget every 2 bps — so a
    tape that is running is met with something close to {!Twap} and a still
    one is worked passively. Damping on the move rather than on the distance
    already travelled is what makes it a brake rather than a post-mortem: by
    the time the distance is large the cost has been paid.

    That damping is the algorithm's only {e market}-driven response; the
    drift budget's taper is a clock. Measured over this catalogue it is worth
    little either way — the sessions here are ordinary, and the tail it
    exists for is a gap day none of them contain. It is kept for the reason a
    desk would keep it, not because the sweep asked for it.

    At [patience = 0] the budget is zero at every minute, no order ever
    rests, and this is {!Twap} share for share — the same limiting-case
    identity {!Implementation_shortfall} has at zero urgency, and it is
    checked the same way.

    Resting orders keep their queue position: an order already at the price
    the algorithm wants is left alone, and only one the market has moved past
    is pulled and rewritten. Crossing pulls everything first, so the shares a
    passive order was sitting on are available to trade now.

    Because it rests, this is the first algorithm whose orders can fill
    between the decision and the submission — {!Execlab_simulation.Driver}
    clamps a submission to what the parent can still take for exactly that
    reason.

    {[
      Driver.run ~algorithm:(Adaptive.create ~patience:0.5 ()) ...
    ]} *)

open! Core
open! Execlab_types

(** [patience] runs from 0 (never rest — {!Twap}) to 1 (rest up to a quarter
    of the parent while the window is young). Values outside that range are
    not rejected: they scale the drift budget linearly, and a negative one
    simply floors it at zero. *)
val create : patience:float -> unit -> Algorithm_intf.t
