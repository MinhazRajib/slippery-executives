(** The naive baseline: submit the parent's entire remaining quantity as one
    market order the first minute it may act, then do nothing.

    This is the "just slam it in" strategy every algorithm is measured
    against — execution value-add is an algorithm's net P&L minus this
    baseline's. It pays the full square-root impact of the whole size at
    once, which is exactly the cost the real algorithms exist to avoid. *)

open! Core
include Algorithm_intf.S
