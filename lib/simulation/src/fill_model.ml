open! Core
open! Execlab_types
open! Execlab_market
open! Execlab_execution

module Config = struct
  type t =
    { half_spread : Price.t
    ; max_participation : float
    ; impact_coefficient : Price.t
    }
  [@@deriving sexp_of]

  (* 25c at full participation puts impact at the 10% cap (~5c) in line with
     square-root-law estimates for a liquid large-cap; the previous 10c
     undershot by ~2x. *)
  let default =
    { half_spread = Price.of_int_cents 2
    ; max_participation = 0.1
    ; impact_coefficient = Price.of_int_cents 25
    }
  ;;
end

type t =
  { config : Config.t
  ; current_bar : Market_bar.t option
  ; volume_used : int
  ; next_fill_id : int
  }
[@@deriving sexp_of]

let create config =
  { config; current_bar = None; volume_used = 0; next_fill_id = 1 }
;;

let budget t (bar : Market_bar.t) =
  Float.to_int
    (t.config.max_participation *. Float.of_int (Size.to_int bar.volume))
;;

let available t bar = Int.max 0 (budget t bar - t.volume_used)
let bid t (bar : Market_bar.t) = Price.( - ) bar.open_ t.config.half_spread
let ask t (bar : Market_bar.t) = Price.( + ) bar.open_ t.config.half_spread

let impact t ~fill_size ~(bar : Market_bar.t) =
  Price.of_float_round_nearest
    (Price.to_float t.config.impact_coefficient
     *. Float.sqrt (fill_size // Size.to_int bar.volume))
;;

let synthetic_bbo t =
  Option.map t.current_bar ~f:(fun bar ->
    let size = Size.of_int (available t bar) in
    { Bbo.bid = Some { Level.price = bid t bar; size }
    ; ask = Some { Level.price = ask t bar; size }
    })
;;

let record_fill t ~(child : Child_order.t) ~price ~size ~bar ~liquidity =
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
  ( { t with
      next_fill_id = t.next_fill_id + 1
    ; volume_used = t.volume_used + size
    }
  , fill )
;;

let execute_aggressive t ~(child : Child_order.t) ~bar ~limit =
  let size = Int.min (Size.to_int child.remaining) (available t bar) in
  if size <= 0
  then t, []
  else (
    let raw =
      match child.request.side with
      | Buy -> Price.( + ) (ask t bar) (impact t ~fill_size:size ~bar)
      | Sell -> Price.( - ) (bid t bar) (impact t ~fill_size:size ~bar)
    in
    (* A crossing limit pays the market's price, but never worse than its own
       limit. *)
    let price =
      match limit, child.request.side with
      | None, _ -> raw
      | Some limit, Side.Buy -> Price.min raw limit
      | Some limit, Sell -> Price.max raw limit
    in
    let t, fill =
      record_fill t ~child ~price ~size ~bar ~liquidity:Liquidity.Taker
    in
    t, [ fill ])
;;

let current_bar_exn t ~here =
  match t.current_bar with
  | Some bar -> bar
  | None ->
    raise_s [%message "Fill_model: no bar has been seen yet" (here : string)]
;;

let on_child_order t (child : Child_order.t) =
  let bar = current_bar_exn t ~here:"on_child_order" in
  match child.request.order_type with
  | Market -> execute_aggressive t ~child ~bar ~limit:None
  | Limit limit ->
    let resting_price =
      match child.request.side with Buy -> ask t bar | Sell -> bid t bar
    in
    if Price.is_marketable child.request.side ~price:limit ~resting_price
    then execute_aggressive t ~child ~bar ~limit:(Some limit)
    else t, []
;;

let fill_resting t ~(child : Child_order.t) ~(bar : Market_bar.t) =
  match child.request.order_type with
  | Market ->
    raise_s
      [%message
        "Fill_model: a market order cannot rest"
          ~order_id:(child.id : Order_id.t)]
  | Limit limit ->
    let traded_through =
      match child.request.side with
      | Buy -> Price.( < ) bar.low limit
      | Sell -> Price.( > ) bar.high limit
    in
    let size = Int.min (Size.to_int child.remaining) (available t bar) in
    if (not traded_through) || size <= 0
    then t, []
    else (
      let t, fill =
        record_fill
          t
          ~child
          ~price:limit
          ~size
          ~bar
          ~liquidity:Liquidity.Maker
      in
      t, [ fill ])
;;

let on_bar_advance t ~bar ~resting_orders =
  let t = { t with current_bar = Some bar; volume_used = 0 } in
  List.fold resting_orders ~init:(t, []) ~f:(fun (t, fills) child ->
    let t, new_fills = fill_resting t ~child ~bar in
    t, fills @ new_fills)
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
