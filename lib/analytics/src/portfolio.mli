<<<<<<< Updated upstream
(** The run's ledger: cash and positions, driven by the stream of {!Fill}s.

    Pure accounting, no judgment — every fill moves cash and one position;
    queries report what you hold, what it cost on average, and profit split
    into realized (locked in by closing trades) and unrealized (open
    positions marked to a current price).

    The same module is fed by every kind of run: the real algorithm's fills,
    a naive baseline's fills, and the all-at-arrival-price fills that define
    gross theoretical P&L — that is what makes "algo net minus baseline net"
    a fair comparison.

    Cash, cost basis, and P&L are [int] cents (named [*_cents]), matching
    {!Fill.notional_cents}; a dedicated [Money.t] is an open question in
    [context.md]. Share counts are signed [int]s: positive long, negative
    short ({!Size.t} is deliberately non-negative, and a position is not an
    order quantity).

    Bookkeeping rules:
    - Buys blend into the average cost; sells never change it (they realize
      P&L against it).
    - A fill that crosses through flat (long 100, sell 250) is split: close
      the 100, then open a 150 short at the fill price.
    - Closing part of a position releases a proportional, rounded share of
      the basis; rounding only shifts a cent between realized-now and
      still-open basis, so the identity below always holds to the cent.

    The identity (enforced by expect tests):

    {[
      equity_cents t ~mark - starting_cash_cents t
      = realized_pnl_cents t + total unrealized at the same marks
    ]} *)
=======
(** The simulator's wallet: tracks cash and the shares we hold, updated by
    every {!Fill}.

    All money is [int] cents (like {!Price.t} and {!Fill.notional_cents}):
    integers add exactly, so the books always balance to the cent.

    This slice only handles buying. Selling — where profit or loss gets
    calculated — is the next slice, and raises until then. *)
>>>>>>> Stashed changes

open! Core
open! Execlab_types

type t [@@deriving sexp_of]

<<<<<<< Updated upstream
(** One-line dashboard summary, e.g.
    ["cash $98450.00, realized $100.00; NVDA +150 @ $11.00"]. *)
val to_string : t -> string

val create : starting_cash_cents:int -> t

(** Record one execution: cash moves by the fill's notional (out on a buy, in
    on a sell) and the symbol's position updates per the rules above. Fills
    come from a fill engine, so a non-positive size is a library-internal
    precondition violation and raises. *)
val apply_fill : t -> Fill.t -> unit

(** {2 Queries} *)
=======
(** A wallet with only cash in it, no shares yet. *)
val create : starting_cash_cents:int -> t

(** Record one trade. Buying: cash goes down, shares go up. Selling: not
    implemented yet — raises. *)
val apply_fill : t -> Fill.t -> unit

(** {2 Questions you can ask} *)
>>>>>>> Stashed changes

val starting_cash_cents : t -> int
val cash_cents : t -> int

<<<<<<< Updated upstream
(** Signed shares held: positive long, negative short, [0] if flat. *)
val position : t -> Symbol.t -> int

(** Signed cost of acquiring the open position: cents paid for a long,
    negative (cents received) for a short. [0] if flat. *)
val cost_basis_cents : t -> Symbol.t -> int

(** Average cost per share in float dollars ([None] if flat) — tradable
    prices are {!Price.t}, statistics about prices are floats, same
    convention as {!Day_stats}. *)
val average_cost : t -> Symbol.t -> float option

val realized_pnl_cents : t -> int

(** [position * mark - cost basis]; [0] if flat. *)
val unrealized_pnl_cents : t -> Symbol.t -> mark:Price.t -> int

(** Cash plus every open position valued at [mark symbol]. *)
val equity_cents : t -> mark:(Symbol.t -> Price.t) -> int
=======
(** How many shares of [symbol] we hold; [0] if none. *)
val shares : t -> Symbol.t -> int

(** Total cash spent buying the shares we hold; [0] if none. *)
val total_cost_cents : t -> Symbol.t -> int

(** Average price paid per share, in dollars (e.g. [Some 11.] after buying
    100 @ $10 and 100 @ $12); [None] if we hold none. *)
val average_cost : t -> Symbol.t -> float option
>>>>>>> Stashed changes
