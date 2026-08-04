(** Engine B: a synthetic exchange behind the same
    {!Execlab_simulation.Engine_intf} seam as the bar-based
    {!Execlab_simulation.Fill_model}. Background agents are calibrated to
    each historical bar — three market-maker archetypes ladder both sides of
    a {!Book} around the bar's price path with depth proportional to bar
    volume and spreads that widen with bar range, while noise traders print a
    tape that tracks the bar's own volume and direction. Client orders walk
    the resting ladders through real price levels, so partial fills, depth
    exhaustion, and market impact {e emerge mechanically} — and because
    makers requote around the historical path each bar, impact is real but
    temporary (the leash).

    Everything is driven by a pure 32-bit LCG ({!Rng}): the same seed
    produces bit-identical fills natively and in the browser, which the
    server's re-run-to-verify anti-cheat depends on.

    Impact has both halves. The {e temporary} half is the ladder walk itself,
    undone when the makers requote. The {e permanent} half is their memory:
    taking liquidity moves a signed pressure that shifts the quote centre
    with the square root of participation and decays each bar, so a buyer who
    keeps lifting offers finds the offers rising to meet him — information
    leaking into the price, as it does on a real tape. With no client orders
    the pressure stays zero and the calibration is untouched.

    A resting client limit takes a real place in the queue: it records the
    agent size displayed at its price when it arrives, and fills only from
    the flow that would have reached it once that size is served. Improving
    on a price nobody shows puts you at the front; joining a crowded one
    makes you wait.

    Simplifications kept on purpose: the background tape of bar [m] is
    printed when bar [m+1] opens, so client submissions always meet fresh
    ladders at a bar's open, and a resting order's fill is recognized at the
    following bar's boundary. *)

open! Core
open! Execlab_types
open! Execlab_market
open! Execlab_execution
open! Execlab_simulation

module Config : sig
  (** The market's calibration. [rung_range_divisor] sets how wide the makers
      quote: one rung is the bar's range over the divisor, rounded and
      floored at a cent. At 25 that is one to three cents across the bundled
      names' typical minutes — the order of a real large-cap spread, and of
      the bar model's 2c default, without pretending to equal it on any
      particular minute. [permanent_impact_coefficient] is the quote shift at
      full participation, and [pressure_decay] the share of remembered client
      aggression that survives each bar. *)
  type t =
    { seed : int
    ; rung_range_divisor : int
    ; permanent_impact_coefficient : Price.t
    ; pressure_decay : float
    }
  [@@deriving sexp_of]

  val default : t
end

type t

val create : Config.t -> t

val on_bar_advance
  :  t
  -> bar:Market_bar.t
  -> resting_orders:Child_order.t list
  -> t * Fill.t list

val on_child_order : t -> Child_order.t -> t * Fill.t list

(** The exchange packed behind the engine seam — pass to
    {!Execlab_simulation.Driver.run}'s [?engine]. *)
val engine : Config.t -> Engine_intf.t

module For_testing : sig
  (** Volume-weighted average of every print so far — background tape and
      client fills alike. The killer calibration invariant: with no client
      orders at all, this must track the historical day's own VWAP. [None]
      before anything trades. *)
  val sim_vwap : t -> float option

  (** Prints the still-pending final bar's background tape (a real run leaves
      the last bar unprinted). *)
  val finish_day : t -> t

  val book : t -> Book.t
end
