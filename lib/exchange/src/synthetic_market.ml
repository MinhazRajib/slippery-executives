open! Core
open! Execlab_types
open! Execlab_market
open! Execlab_execution
open! Execlab_simulation

module Config = struct
  type t = { seed : int } [@@deriving sexp_of]

  let default = { seed = 1 }
end

(* Three maker archetypes, tight to loose: quote offset in multiples of the
   bar's base half-spread, displayed size as a fraction of the bar's volume.
   Together they ladder ~12% of a bar's volume per side over three levels —
   enough that demo-scale orders trade inside the ladder and outsized ones
   exhaust it. *)
let maker_ladder = [ 1, 0.02; 2, 0.04; 4, 0.06 ]

(* The synthetic tape: six noise prints per bar of ~volume/8 each, biased
   65/35 with the intra-bar move's direction. *)
let noise_slices = 6
let noise_slice_fraction = 1. /. 8.
let noise_volume_jitter = 0.3
let with_move_bias = 0.65

type t =
  { config : Config.t
  ; book : Book.t
  ; rng : Rng.t
  ; current_bar : Market_bar.t option
  ; next_fill_id : int
  ; print_notional : float (* dollars, everything that traded *)
  ; print_volume : int
  }

let create config =
  { config
  ; book = Book.empty
  ; rng = Rng.create ~seed:config.Config.seed
  ; current_bar = None
  ; next_fill_id = 1
  ; print_notional = 0.
  ; print_volume = 0
  }
;;

(* Base half-spread widens with the bar's range: a sixth of it, floored at
   one cent. *)
let base_half_spread_cents (bar : Market_bar.t) =
  Int.max 1 ((Price.to_int_cents bar.high - Price.to_int_cents bar.low) / 6)
;;

let record_print t ~price_cents ~size =
  { t with
    print_notional =
      t.print_notional
      +. (Float.of_int price_cents /. 100. *. Float.of_int size)
  ; print_volume = t.print_volume + size
  }
;;

(* Makers requote both sides fresh around [fundamental]: the leash that pulls
   the market back to the historical path and makes client impact temporary. *)
let requote t ~(bar : Market_bar.t) ~fundamental_cents =
  let half = base_half_spread_cents bar in
  let volume = Size.to_int bar.volume in
  (* A level whose price would fall to zero or below (a penny stock wider
     than its own price) is dropped, not clamped: clamping would pile phantom
     depth onto 1c and could distort the touch. The rng draw still happens
     for every rung so ladders at ordinary prices are unchanged. *)
  let side rng direction =
    let rng, levels =
      List.fold_map maker_ladder ~init:rng ~f:(fun rng (steps, fraction) ->
        let rng, size =
          Rng.jitter
            rng
            ~around:(Float.of_int volume *. fraction)
            ~spread:0.25
        in
        let price_cents = fundamental_cents + (direction * half * steps) in
        rng, (price_cents, Float.to_int size))
    in
    ( rng
    , List.filter_map levels ~f:(fun (price_cents, size) ->
        if price_cents >= 1
        then Some (Price.of_int_cents price_cents, size)
        else None) )
  in
  let rng, asks = side t.rng 1 in
  let rng, bids = side rng (-1) in
  let book =
    Book.set_side (Book.set_side Book.empty ~side:Sell asks) ~side:Buy bids
  in
  { t with rng; book }
;;

(* The conventional intra-bar path: open toward the nearer extreme, across to
   the other, then to the close. *)
let path_points (bar : Market_bar.t) =
  let open_ = Price.to_int_cents bar.open_ in
  let close = Price.to_int_cents bar.close in
  let high = Price.to_int_cents bar.high in
  let low = Price.to_int_cents bar.low in
  let first, second = if close >= open_ then low, high else high, low in
  [ open_; first; second; close ]
;;

(* One bar of background trading: at each step of the path, makers requote
   around the path price and a noise trader crosses the spread. Nothing here
   touches client orders — it exists to put realistic prints on the synthetic
   tape, which is what the zero-client VWAP invariant measures. *)
let trade_background t ~(bar : Market_bar.t) =
  let points = path_points bar in
  let volume = Size.to_int bar.volume in
  let slice_of index =
    let points_count = List.length points in
    List.nth_exn
      points
      (Int.min (points_count - 1) (index * points_count / noise_slices))
  in
  let rec slices t index previous =
    if index >= noise_slices
    then t
    else (
      let fundamental_cents = slice_of index in
      let t = requote t ~bar ~fundamental_cents in
      let rng, size =
        Rng.jitter
          t.rng
          ~around:(Float.of_int volume *. noise_slice_fraction)
          ~spread:noise_volume_jitter
      in
      let rising = fundamental_cents >= previous in
      let rng, buys =
        Rng.bernoulli
          rng
          ~p:(if rising then with_move_bias else 1. -. with_move_bias)
      in
      let t = { t with rng } in
      let taker_side : Side.t = if buys then Buy else Sell in
      let book, prints =
        Book.take t.book ~taker_side ~size:(Float.to_int size) ()
      in
      let t =
        List.fold prints ~init:{ t with book } ~f:(fun t (price, size) ->
          record_print t ~price_cents:(Price.to_int_cents price) ~size)
      in
      slices t (index + 1) fundamental_cents)
  in
  slices t 0 (List.hd_exn points)
;;

let make_fill t ~(child : Child_order.t) ~price ~size ~liquidity =
  let bar =
    match t.current_bar with
    | Some bar -> bar
    | None -> raise_s [%message "Synthetic_market: no bar has been seen yet"]
  in
  let fill =
    { Fill.fill_id = t.next_fill_id
    ; symbol = child.request.symbol
    ; price
    ; size = Size.of_int size
    ; order_id = child.id
    ; side = child.request.side
    ; time = bar.Market_bar.time
    ; liquidity
    }
  in
  let t = { t with next_fill_id = t.next_fill_id + 1 } in
  let t =
    record_print t ~price_cents:(Price.to_int_cents fill.price) ~size
  in
  t, fill
;;

(* Client resting limits keep Engine A's strict-through convention (the book
   only holds agent liquidity in v1, so there is no queue to model): if the
   bar trades strictly through the limit, the whole remainder fills there as
   a maker. *)
let fill_resting t ~(child : Child_order.t) ~(bar : Market_bar.t) =
  match child.request.order_type with
  | Market ->
    raise_s
      [%message
        "Synthetic_market: a market order cannot rest"
          ~order_id:(child.id : Order_id.t)]
  | Limit limit ->
    let traded_through =
      match child.request.side with
      | Buy -> Price.( < ) bar.low limit
      | Sell -> Price.( > ) bar.high limit
    in
    if not traded_through
    then t, []
    else (
      let t, fill =
        make_fill
          t
          ~child
          ~price:limit
          ~size:(Size.to_int child.remaining)
          ~liquidity:Liquidity.Maker
      in
      t, [ fill ])
;;

let on_bar_advance t ~bar ~resting_orders =
  (* Background trading of the previous bar happens conceptually before this
     bar opens; its prints are already in the stats. Then: fresh ladders
     around this bar's open — client submissions this bar walk these, and
     their dents last until the next requote (impact is real but temporary). *)
  let t =
    match t.current_bar with
    | None -> t
    | Some previous -> trade_background t ~bar:previous
  in
  let t = { t with current_bar = Some bar } in
  let t =
    requote
      t
      ~bar
      ~fundamental_cents:(Price.to_int_cents bar.Market_bar.open_)
  in
  List.fold resting_orders ~init:(t, []) ~f:(fun (t, fills) child ->
    let t, new_fills = fill_resting t ~child ~bar in
    t, fills @ new_fills)
;;

let on_child_order t (child : Child_order.t) =
  let taker_side = child.request.side in
  let limit =
    match child.request.order_type with
    | Market -> None
    | Limit limit -> Some limit
  in
  let book, prints =
    Book.take
      t.book
      ~taker_side
      ?limit
      ~size:(Size.to_int child.remaining)
      ()
  in
  let t = { t with book } in
  List.fold prints ~init:(t, []) ~f:(fun (t, fills) (price, size) ->
    let t, fill =
      make_fill t ~child ~price ~size ~liquidity:Liquidity.Taker
    in
    t, fills @ [ fill ])
;;

let engine config =
  Engine_intf.Packed
    ( (module struct
        type nonrec t = t

        let on_bar_advance = on_bar_advance
        let on_child_order = on_child_order
      end)
    , create config )
;;

module For_testing = struct
  let sim_vwap t =
    if t.print_volume = 0
    then None
    else Some (t.print_notional /. Float.of_int t.print_volume)
  ;;

  let finish_day t =
    match t.current_bar with
    | None -> t
    | Some bar -> trade_background t ~bar
  ;;

  let book t = t.book
end
