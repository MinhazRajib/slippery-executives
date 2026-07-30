(** The per-instruction cost decomposition: everything the platform judges an
    execution on, computed from an instruction, its fills, and the
    {!Benchmarks} prices.

    Pure arithmetic — no market data, no state. {!Benchmarks} owns where the
    reference prices come from; {!Portfolio} owns run-level cash and
    positions; this module grades one instruction's fills against the
    benchmarks it is handed, so it can grade any run the same way: the real
    algorithm's fills, a naive baseline's, or none at all.

    Sign conventions:
    - bps metrics are side-adjusted: positive always means "worse than the
      benchmark" (a buy that paid more, a sell that received less), so
      reports for buys and sells read the same way;
    - [*_pnl_cents] are plain profit, positive when money was made;
    - [*_cost_cents] are plain cost, positive when money was lost.

    The identity every grading satisfies (enforced by expect tests):

    {[
      gross_theoretical_pnl_cents
      = net_pnl_cents + friction_cost_cents + opportunity_cost_cents
    ]}

    Gross is what the alpha deserved (instant, free, full fill at the arrival
    price); friction is what execution paid on the shares that did fill;
    opportunity is what the unfilled remainder never earned. Spread cost is
    an {e attribution} of part of the friction (the half-spread tolls on
    liquidity-taking fills, minus rebates on providing ones), not an extra
    term — the full spread/impact/timing split of friction is the roadmap's
    later metric tree. *)

open! Core
open! Execlab_types

module Fill_metrics : sig
  (** Statistics that only exist once at least one share has filled; [None]
      on the grading of an entirely unfilled instruction. *)
  type t =
    { average_fill_price : float
    (** Size-weighted average execution price, in dollars. *)
    ; shortfall_bps : float
    (** Implementation shortfall: average fill price vs. the arrival price,
        side-adjusted — the headline "how much did execution friction eat"
        number. *)
    ; vwap_slippage_bps : float
    (** Average fill price vs. the day's VWAP, side-adjusted: did you trade
        worse than the crowd? *)
    }
  [@@deriving sexp_of]
end

type t =
  { symbol : Symbol.t
  ; side : Side.t
  ; quantity : Size.t (** Shares the instruction asked for. *)
  ; filled : Size.t (** Shares actually executed. *)
  ; completion_rate : float (** [filled / quantity], from 0 to 1. *)
  ; arrival_price : Price.t (** See {!Benchmarks.arrival_price}. *)
  ; terminal_price : Price.t (** See {!Benchmarks.terminal_price}. *)
  ; day_vwap : float (** {!Day_stats.vwap} of the session, in dollars. *)
  ; fill_metrics : Fill_metrics.t option
  ; spread_cost_cents : int
  (** Half-spread paid on taking fills minus rebates earned on providing ones
      — the part of the friction attributable to crossing spreads. *)
  ; friction_cost_cents : int
  (** Implementation shortfall in cents: what the filled shares cost beyond
      filling them all at the arrival price. *)
  ; opportunity_cost_cents : int
  (** What the unfilled shares would have earned at the arrival price and
      been worth at the terminal price — pure lost alpha when the instruction
      was right, negative (a lucky save) when it was wrong. *)
  ; gross_theoretical_pnl_cents : int
  (** The alpha's deserved P&L: the full quantity filled instantly at the
      arrival price with zero cost, valued at the terminal price. *)
  ; net_pnl_cents : int
  (** What actually happened: the filled shares valued at the terminal price,
      minus what they really cost. *)
  ; alpha_capture : float option
  (** [net / gross], the headline score; [None] unless gross is positive (a
      wrong-way alpha makes the ratio meaningless). *)
  }
[@@deriving sexp_of]

(** Grade [fills] — the output of executing [instruction] against some market
    — against the benchmark prices. Empty [fills] is a legitimate grading
    (nothing filled: all opportunity cost, no fill metrics). Errors if a
    fill's symbol or side is not the instruction's, or if the fills sum to
    more than the instruction's quantity.

    [half_spread] is the fill model's synthetic half-spread (see context.md):
    {!Fill.t} records {e that} a fill took liquidity, not the spread it
    crossed, so spread attribution needs the run's config knob. When the fill
    model later scales spread with bar range, this becomes a per-fill lookup. *)
val create
  :  instruction:Alpha_instruction.t
  -> fills:Fill.t list
  -> arrival_price:Price.t
  -> terminal_price:Price.t
  -> day_vwap:float
  -> half_spread:Price.t
  -> t Or_error.t

(** Execution value-add: [algo]'s net P&L minus [baseline]'s, in cents —
    positive means the algorithm beat the baseline. Errors unless both
    gradings are of the same instruction against the same benchmarks
    (comparing anything else is comparing alphas, not executions). *)
val value_add_cents : algo:t -> baseline:t -> int Or_error.t
