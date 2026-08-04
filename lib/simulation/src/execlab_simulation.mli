(** The simulation layer: where orders meet the market.

    {!Fill_model} is Engine A, the bar-based fill model; {!Driver} conducts
    the whole loop — bars, algorithms, the order manager, and fills.
    Re-export each new module here so the whole library is reachable through
    the top-level {!Execlab_simulation} module. *)

module Driver = Driver
module Engine_intf = Engine_intf
module Fill_model = Fill_model
