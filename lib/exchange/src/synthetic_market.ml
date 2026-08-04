open! Core
open! Execlab_types
open! Execlab_market
open! Execlab_execution
open! Execlab_simulation

module Config = struct
  type t =
    { seed : int
    ; rung_range_divisor : int
    ; permanent_impact_coefficient : Price.t
    ; pressure_decay : float
    }
  [@@deriving sexp_of]

  (* A rung of the bar's range over 30 puts the touch of a median large-cap
     minute (~70c of range) about 2c either side of the fundamental — the
     same spread Fill_model quotes by default, so the two engines agree about
     what an ordinary trade costs and differ only where they should: what
     happens when you want size. *)
  let default =
    { seed = 1
    ; rung_range_divisor = 30
    ; permanent_impact_coefficient = Price.of_int_cents 15
    ; pressure_decay = 0.6
    }
  ;;
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
  ; client_pressure : float
      (* signed participation clients have taken — their share of the volume
         of the bar they actually traded in — decayed each bar: what the
         makers remember about being run over. Shares would be the wrong
         unit: 76,000 of them is a beating in a thin minute and a rounding
         error in a busy one. *)
  ; queues : int Order_id.Map.t
      (* resting client order -> agent shares still ahead of it at its price,
         counted when it was posted *)
  ; flow : (Side.t * int * int) list
  (* the completed bar's noise prints as (taker side, price cents, size): the
     trading a resting client order could have joined *)
  }

let create config =
  { config
  ; book = Book.empty
  ; rng = Rng.create ~seed:config.Config.seed
  ; current_bar = None
  ; next_fill_id = 1
  ; print_notional = 0.
  ; print_volume = 0
  ; client_pressure = 0.
  ; queues = Order_id.Map.empty
  ; flow = []
  }
;;

(* One rung of the ladder: the bar's range scaled down by the config's
   divisor, floored at a cent. Wider bars quote wider, exactly as makers
   widen when the tape turns violent. *)
let rung_cents t (bar : Market_bar.t) =
  Int.max
    1
    ((Price.to_int_cents bar.high - Price.to_int_cents bar.low)
     / t.config.Config.rung_range_divisor)
;;

(* What the makers have concluded from being traded against: a square-root
   shift of the quote centre in the direction of the client's flow. This is
   the {e permanent} half of impact — it survives the rebuilding of the
   ladders and fades over bars instead — so a buyer who keeps lifting offers
   finds the offers rising to meet him. *)
let pressure_shift_cents t (_ : Market_bar.t) =
  if Float.( = ) t.client_pressure 0.
  then 0
  else (
    let magnitude =
      Float.of_int (Price.to_int_cents t.config.permanent_impact_coefficient)
      *. Float.sqrt (Float.abs t.client_pressure)
    in
    (if Float.( > ) t.client_pressure 0. then 1 else -1)
    * Float.iround_nearest_exn magnitude)
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
  let half = rung_cents t bar in
  let fundamental_cents =
    Int.max 1 (fundamental_cents + pressure_shift_cents t bar)
  in
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
          let price_cents = Price.to_int_cents price in
          let t = record_print t ~price_cents ~size in
          (* Remember the print's side and price: this is the flow a resting
             client order could have been part of. *)
          { t with flow = (taker_side, price_cents, size) :: t.flow })
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

(* A resting client order sits in the queue at its price: it fills only once
   the flow that could have hit it exceeds the agent size that was displayed
   ahead of it when it arrived. Buys are hit by noise sells at or below the
   limit, sells lifted by noise buys at or above — so being early, or
   improving on a price nobody else shows, is worth exactly what it should
   be. *)
let servable_volume t ~(child : Child_order.t) ~limit =
  let limit_cents = Price.to_int_cents limit in
  List.sum (module Int) t.flow ~f:(fun (taker_side, price_cents, size) ->
    match child.request.side, taker_side with
    | Side.Buy, Side.Sell when price_cents <= limit_cents -> size
    | Sell, Buy when price_cents >= limit_cents -> size
    | (Buy | Sell), (Buy | Sell) -> 0)
;;

let fill_resting t ~claimed ~(child : Child_order.t) =
  match child.request.order_type with
  | Market ->
    raise_s
      [%message
        "Synthetic_market: a market order cannot rest"
          ~order_id:(child.id : Order_id.t)]
  | Limit limit ->
    let ahead = Option.value (Map.find t.queues child.id) ~default:0 in
    (* The bar traded a finite amount: whatever better-priced resting orders
       have already claimed is gone, and cannot fill this one too. *)
    let served = Int.max 0 (servable_volume t ~child ~limit - claimed) in
    let queue_drain = Int.min ahead served in
    let ours =
      Int.min (Size.to_int child.remaining) (served - queue_drain)
    in
    let t =
      { t with
        queues = Map.set t.queues ~key:child.id ~data:(ahead - queue_drain)
      }
    in
    if ours <= 0
    then t, claimed + queue_drain, []
    else (
      let t, fill =
        make_fill t ~child ~price:limit ~size:ours ~liquidity:Liquidity.Maker
      in
      t, claimed + queue_drain + ours, [ fill ])
;;

(* Price priority, then arrival: the best-priced resting order gets first
   claim on the bar's flow, and what it takes is not there for the next. Buys
   and sells draw on opposite flows, so each side keeps its own running
   claim. *)
let in_priority_order resting_orders =
  let limit_cents (child : Child_order.t) =
    match child.request.order_type with
    | Limit limit -> Price.to_int_cents limit
    | Market -> 0
  in
  let buys, sells =
    List.partition_tf resting_orders ~f:(fun (child : Child_order.t) ->
      match child.request.side with Buy -> true | Sell -> false)
  in
  List.stable_sort buys ~compare:(fun a b ->
    Int.compare (limit_cents b) (limit_cents a))
  @ List.stable_sort sells ~compare:(fun a b ->
    Int.compare (limit_cents a) (limit_cents b))
;;

let on_bar_advance t ~bar ~resting_orders =
  (* The previous bar's background trading happens conceptually before this
     bar opens: it prints that bar's tape and leaves behind the flow a
     resting order could have joined. Then the makers thin their memory of
     client aggression, rebuild ladders around this bar's open shifted by
     whatever pressure remains, and the orders still resting take their turn
     in the queue. *)
  let t = { t with flow = [] } in
  let t =
    match t.current_bar with
    | None -> t
    | Some previous -> trade_background t ~bar:previous
  in
  let t =
    { t with
      current_bar = Some bar
    ; client_pressure = t.client_pressure *. t.config.Config.pressure_decay
    ; queues =
        Map.filter_keys t.queues ~f:(fun id ->
          List.exists resting_orders ~f:(fun (child : Child_order.t) ->
            Order_id.equal child.id id))
    }
  in
  let t =
    requote
      t
      ~bar
      ~fundamental_cents:(Price.to_int_cents bar.Market_bar.open_)
  in
  let (t, (_ : int), (_ : int)), fills =
    List.fold_map
      (in_priority_order resting_orders)
      ~init:(t, 0, 0)
      ~f:(fun (t, buy_claimed, sell_claimed) (child : Child_order.t) ->
        match child.request.side with
        | Buy ->
          let t, buy_claimed, fills =
            fill_resting t ~claimed:buy_claimed ~child
          in
          (t, buy_claimed, sell_claimed), fills
        | Sell ->
          let t, sell_claimed, fills =
            fill_resting t ~claimed:sell_claimed ~child
          in
          (t, buy_claimed, sell_claimed), fills)
  in
  t, List.concat fills
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
  let taken = List.sum (module Int) prints ~f:snd in
  (* Taking liquidity is the aggression the makers react to; whatever a limit
     could not take rests, joining the queue behind the size displayed at its
     price right now. *)
  let t =
    { t with
      book
    ; client_pressure =
        (* Participation is measured against the bar the shares were actually
           taken from, and the running total is clamped, so the remembered
           aggression stays bounded by the coefficient however hard a client
           leans. *)
        (let participation =
           match t.current_bar with
           | None -> 0.
           | Some bar ->
             let volume = Size.to_int bar.Market_bar.volume in
             if volume <= 0 then 0. else taken // volume
         in
         Float.clamp_exn
           ~min:(-1.)
           ~max:1.
           (t.client_pressure
            +. (Float.of_int (Side.sign child.request.side) *. participation)
           ))
    }
  in
  let t =
    match limit, Size.to_int child.remaining - taken with
    | Some limit, resting when resting > 0 ->
      { t with
        queues =
          Map.set
            t.queues
            ~key:child.id
            ~data:(Book.size_at t.book ~side:child.request.side ~price:limit)
      }
    | (Some (_ : Price.t) | None), (_ : int) -> t
  in
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
