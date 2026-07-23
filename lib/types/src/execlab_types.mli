(** Core domain types shared across the execution platform.

    These are the vocabulary types every other component speaks: {!Symbol}
    identifies an instrument, {!Side} says buy or sell, {!Price} is a
    fixed-point price in cents, {!Size} is an order quantity, and
    {!Alpha_instruction} is one parsed row of an uploaded alpha-output file.
    Re-export each new module here so the whole library is reachable through
    the top-level {!Execlab_types} module. *)

module Alpha_instruction = Alpha_instruction
module Price = Price
module Side = Side
module Size = Size
module Symbol = Symbol
module Order_id = Order_id
module Level = Level
module Liquidity = Liquidity
module Bbo = Bbo
module Fill = Fill
module Order_type = Order_type
module Time_in_force = Time_in_force
