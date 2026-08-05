open! Core
open! Execlab_types

module Position = struct
<<<<<<< Updated upstream
  (* [shares] is signed (positive long, negative short); [basis_cents] is the
     signed cost of acquiring it, so [basis_cents / shares] is the average
     cost per share for longs and shorts alike. A position at zero shares is
     removed from the table rather than kept. *)
  type t =
    { mutable shares : int
    ; mutable basis_cents : int
    }
  [@@deriving sexp_of]
=======
  (* What we hold of one stock: how many shares, and the total cash we spent
     buying them. Total spent / shares = average price paid. *)
  type t =
    { mutable shares : int
    ; mutable total_cost_cents : int
    }
  [@@deriving sexp_of]

  
>>>>>>> Stashed changes
end

type t =
  { starting_cash_cents : int
  ; mutable cash_cents : int
<<<<<<< Updated upstream
  ; mutable realized_pnl_cents : int
=======
>>>>>>> Stashed changes
  ; positions : Position.t Hashtbl.M(Symbol).t
  }
[@@deriving sexp_of]

let create ~starting_cash_cents =
  { starting_cash_cents
  ; cash_cents = starting_cash_cents
<<<<<<< Updated upstream
  ; realized_pnl_cents = 0
=======
>>>>>>> Stashed changes
  ; positions = Hashtbl.create (module Symbol)
  }
;;

let starting_cash_cents t = t.starting_cash_cents
let cash_cents t = t.cash_cents
<<<<<<< Updated upstream
let realized_pnl_cents t = t.realized_pnl_cents

let position t symbol =
=======

let shares t symbol =
>>>>>>> Stashed changes
  match Hashtbl.find t.positions symbol with
  | None -> 0
  | Some (position : Position.t) -> position.shares
;;

<<<<<<< Updated upstream
let cost_basis_cents t symbol =
  match Hashtbl.find t.positions symbol with
  | None -> 0
  | Some (position : Position.t) -> position.basis_cents
=======
let total_cost_cents t symbol =
  match Hashtbl.find t.positions symbol with
  | None -> 0
  | Some (position : Position.t) -> position.total_cost_cents
>>>>>>> Stashed changes
;;

let average_cost t symbol =
  match Hashtbl.find t.positions symbol with
  | None -> None
<<<<<<< Updated upstream
  | Some { shares; basis_cents } ->
    Some (Float.of_int basis_cents /. 100. /. Float.of_int shares)
;;

let unrealized_pnl_cents t symbol ~mark =
  match Hashtbl.find t.positions symbol with
  | None -> 0
  | Some { shares; basis_cents } ->
    (shares * Price.to_int_cents mark) - basis_cents
;;

let equity_cents t ~mark =
  Hashtbl.fold
    t.positions
    ~init:t.cash_cents
    ~f:(fun ~key:symbol ~data:{ shares; basis_cents = _ } acc ->
      acc + (shares * Price.to_int_cents (mark symbol)))
;;

(* Nearest-cent split of a basis: [by] is positive, the numerator may be
   negative (short basis). Ties round away from zero so longs and shorts are
   treated symmetrically. *)
let divide_rounding_to_nearest numerator ~by =
  let rounded = (abs numerator + (by / 2)) / by in
  if numerator < 0 then -rounded else rounded
;;

let apply_fill t (fill : Fill.t) =
  if Size.( <= ) fill.size Size.zero
  then
    raise_s
      [%message
        "Portfolio.apply_fill: fill size must be positive" (fill : Fill.t)];
  let delta = Side.sign fill.side * Size.to_int fill.size in
  let price_cents = Price.to_int_cents fill.price in
  t.cash_cents <- t.cash_cents - (delta * price_cents);
  let position =
    Hashtbl.find_or_add t.positions fill.symbol ~default:(fun () ->
      { Position.shares = 0; basis_cents = 0 })
  in
  let increases_position =
    position.shares = 0 || Bool.equal (position.shares > 0) (delta > 0)
  in
  if increases_position
  then (
    position.shares <- position.shares + delta;
    position.basis_cents <- position.basis_cents + (delta * price_cents))
  else (
    (* The fill trades against the position: close up to the whole position,
       realizing P&L against the released slice of basis, then any leftover
       opens a fresh position at the fill price. *)
    let held = abs position.shares in
    let held_sign = Int.sign position.shares |> Sign.to_int in
    let closed = Int.min (abs delta) held in
    let released_basis_cents =
      if closed = held
      then position.basis_cents
      else
        divide_rounding_to_nearest (position.basis_cents * closed) ~by:held
    in
    t.realized_pnl_cents
    <- t.realized_pnl_cents
       + ((closed * price_cents * held_sign) - released_basis_cents);
    position.shares <- position.shares - (closed * held_sign);
    position.basis_cents <- position.basis_cents - released_basis_cents;
    let leftover = delta + (closed * held_sign) in
    if leftover <> 0
    then (
      position.shares <- leftover;
      position.basis_cents <- leftover * price_cents));
  if position.shares = 0 then Hashtbl.remove t.positions fill.symbol
;;

let dollar_string cents =
  let magnitude = Price.to_string_dollar (Price.of_int_cents (abs cents)) in
  if cents < 0 then "-" ^ magnitude else magnitude
;;

let to_string t =
  let positions =
    match
      Hashtbl.to_alist t.positions
      |> List.sort ~compare:(fun (a, _) (b, _) -> Symbol.compare a b)
    with
    | [] -> "no positions"
    | positions ->
      List.map
        positions
        ~f:(fun (symbol, { Position.shares; basis_cents }) ->
          (* [basis_cents] and [shares] always share a sign, so the average
             cost per share is positive; the sign is shown on the count. *)
          let average =
            dollar_string
              (divide_rounding_to_nearest (abs basis_cents) ~by:(abs shares))
          in
          let shares =
            if shares > 0
            then [%string "+%{shares#Int}"]
            else Int.to_string shares
          in
          [%string "%{symbol#Symbol} %{shares} @ %{average}"])
      |> String.concat ~sep:", "
  in
  [%string
    "cash %{dollar_string t.cash_cents}, realized %{dollar_string \
     t.realized_pnl_cents}; %{positions}"]
=======
  | Some { shares; total_cost_cents } ->
    Some (Float.of_int total_cost_cents /. 100. /. Float.of_int shares)
;;

let apply_fill t (fill : Fill.t) =
  match fill.side with
  | Buy ->
    let cost_cents = Fill.notional_cents fill in
    t.cash_cents <- t.cash_cents - cost_cents;
    let position =
      Hashtbl.find_or_add t.positions fill.symbol ~default:(fun () ->
        { Position.shares = 0; total_cost_cents = 0 })
    in
    position.shares <- position.shares + Size.to_int fill.size;
    position.total_cost_cents <- position.total_cost_cents + cost_cents
  | Sell ->
>>>>>>> Stashed changes
;;
