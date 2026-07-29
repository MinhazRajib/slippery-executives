(** Terminal rendering of {!Transaction_cost} gradings — the results screen
    of the MVP, until the GUI exists.

    Layout mirrors the final-screen sketch in [context.md]: the fill story
    first (what executed, at what average, vs. which benchmarks), then the
    P&L story (gross alpha, net, opportunity, alpha captured). *)

open! Core
open! Execlab_types

(** One grading as a small terminal block, e.g.:

    {[
      NVDA BUY 100 shares
        filled          100 of 100 (100.0%)
        arrival         $10.00
        terminal        $11.00
        day vwap        $10.1500
        avg fill        $10.2000
        shortfall       +200.0 bps ($20.00)
        vs day vwap     +49.3 bps
        spread cost     $1.00
        gross alpha     $100.00
        net P&L         $80.00
        opportunity     $0.00
        alpha captured  80.0%
    ]} *)
val to_string_hum : Transaction_cost.t -> string

(** Both gradings rendered under their names, followed by the execution
    value-add line ([algo] net minus [baseline] net — see
    {!Transaction_cost.value_add_cents}, which supplies the error when the
    gradings are not comparable). *)
val comparison
  :  algo:Transaction_cost.t
  -> algo_name:string
  -> baseline:Transaction_cost.t
  -> baseline_name:string
  -> string Or_error.t
