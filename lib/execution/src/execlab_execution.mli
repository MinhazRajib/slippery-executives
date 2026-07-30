(** Execution machinery: the trading desk between alpha instructions and the
    fill engine.

    {!Parent_order} tracks one instruction's lifecycle; algorithms
    implementing {!Algorithm_intf.S} (like {!Twap}) slice it into
    {!Child_order}s; {!Order_manager} coordinates and enforces the
    invariants. Re-export each new module here so the whole library is
    reachable through the top-level {!Execlab_execution} module. *)

module Algorithm_intf = Algorithm_intf
module Cancel_reason = Cancel_reason
module Child_order = Child_order
module Immediate = Immediate
module Order_manager = Order_manager
module Parent_order = Parent_order
module Twap = Twap
