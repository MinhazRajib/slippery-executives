(** Engine B: the synthetic exchange. {!Book} holds the agent ladders, {!Rng}
    makes every run reproducible cross-platform, and {!Synthetic_market} is
    the engine itself — plug it into the driver via
    {!Synthetic_market.engine}. *)

module Book = Book
module Rng = Rng
module Synthetic_market = Synthetic_market
