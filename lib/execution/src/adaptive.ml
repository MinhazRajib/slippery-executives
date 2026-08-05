open! Core
open! Execlab_types
open! Execlab_market

(* The most drift, as a fraction of the parent, that full patience will
   tolerate at the start of the window. Named because it sets how different
   this algorithm can be from {!Twap} at all: at 0 it is Twap. *)
let max_drift_fraction = 0.25

(* How fast the market has to be running against an order before patience is
   worth half as much. Waiting is cheap only while the price is not moving
   away: once it is, every minute spent resting is repriced into a worse
   cross later. The signal is the last bar's move, not the distance already
   travelled from the decision price — a level says what waiting has already
   cost, a move says what the next minute of it will. Two basis points in a
   minute is a market going somewhere. *)
let adverse_half_life_bps = 2.

let create ~patience () : Algorithm_intf.t =
  (module struct
    (* The tape as of the previous decision, so the algorithm can tell a
       market that is moving from one that has moved. [None] on the first
       bar, where there is no move to measure yet. *)
    type state = Price.t option

    let name = "adaptive"
    let init ~parent:(_ : Parent_order.t) = None
    let minutes between = Float.to_int (Time_ns.Span.to_min between)

    (* Signed distance of the tape from the decision price, in basis points,
       positive when the market has moved in this order's favor: a buy is in
       front when the tape is below its arrival price. *)
    let edge_bps ~(side : Side.t) ~last ~arrival =
      match arrival with
      | None -> 0.
      | Some arrival ->
        let arrival = Price.to_float arrival in
        if Float.( <= ) arrival 0.
        then 0.
        else
          Float.of_int (Side.sign side)
          *. (arrival -. Price.to_float last)
          /. arrival
          *. 10_000.
    ;;

    let request ~instruction ~quantity ~order_type ~time_in_force =
      Or_error.ok_exn
        (Child_order.Request.create
           ~symbol:instruction.Alpha_instruction.symbol
           ~side:instruction.Alpha_instruction.side
           ~quantity:(Size.of_int quantity)
           ~order_type
           ~time_in_force)
    ;;

    (* One decision per bar, in three cases: cross what the schedule is owed,
       take the schedule aggressively because the price is good, or rest
       inside the drift budget and let the market come to us. *)
    let on_bar previous_last (context : Algorithm_intf.Context.t) =
      let parent = context.parent in
      let instruction = parent.instruction in
      let side = instruction.Alpha_instruction.side in
      let arrival = instruction.Alpha_instruction.arrival_time in
      let deadline = instruction.Alpha_instruction.deadline in
      let total_minutes = minutes (Time_ns.Ofday.diff deadline arrival) in
      let elapsed_minutes =
        minutes (Time_ns.Ofday.diff context.now arrival)
      in
      let minutes_left = Int.max 0 (total_minutes - elapsed_minutes) in
      let total = Size.to_int (Parent_order.total parent) in
      let filled = Size.to_int parent.filled in
      (* Twap's schedule, share for share: the trajectory this algorithm
         adapts around rather than replaces. *)
      let target =
        if total_minutes = 0
        then total
        else total * elapsed_minutes / total_minutes
      in
      (* How far behind the schedule we will tolerate being, tapering to
         nothing at the deadline. Patience buys drift early and none at the
         end, so being passive can delay a fill but never cost a completion. *)
      let last = context.previous_bar.Market_bar.close in
      let edge = edge_bps ~side ~last ~arrival:parent.arrival_price in
      (* Patience is damped by the speed the market is moving away at, so a
         tape running is met with something closer to Twap and a quiet one is
         worked passively. Measuring the move rather than the distance is
         what makes this predictive: by the time the distance is large the
         cost has already been paid. *)
      let drift_bps =
        match previous_last with
        | None -> 0.
        | Some before -> edge_bps ~side ~last ~arrival:(Some before)
      in
      let damping =
        1. /. (1. +. (Float.max 0. (-.drift_bps) /. adverse_half_life_bps))
      in
      let drift_budget =
        if total_minutes = 0
        then 0
        else
          Int.max
            0
            (Float.iround_nearest_exn
               (patience
                *. max_drift_fraction
                *. damping
                *. Float.of_int total
                *. Float.of_int minutes_left
                /. Float.of_int total_minutes))
      in
      (* Rest at the last trade. Whatever the market does next, a fill here
         is better than crossing for it now — crossing pays the far side of
         the spread, this pays the near one — so the price never needs a
         benchmark guard; whether to wait at all is the decision below. *)
      let limit = last in
      (* A resting order already at the right price keeps its place in the
         queue; repricing means going to the back of it, so only orders whose
         price the market has moved past are pulled. *)
      let keep, stale =
        List.partition_tf context.live_orders ~f:(fun child ->
          Order_type.equal child.Child_order.request.order_type (Limit limit))
      in
      let resting =
        List.sum (module Int) keep ~f:(fun child ->
          Size.to_int child.Child_order.remaining)
      in
      let deficit = target - (filled + resting) in
      let favorable = Float.( > ) edge 0. in
      let cancel_all =
        List.map context.live_orders ~f:(fun child ->
          Algorithm_intf.Action.Cancel child.Child_order.id)
      in
      let cancel_stale =
        List.map stale ~f:(fun child ->
          Algorithm_intf.Action.Cancel child.Child_order.id)
      in
      match deficit > drift_budget || (favorable && deficit > 0) with
      | true ->
        (* Cross. Pulling every resting order first makes the size owed a
           question about fills alone, and takes back the shares a passive
           order was sitting on so they can be traded now. *)
        let quantity = Int.min (target - filled) (total - filled) in
        if quantity <= 0
        then Some last, cancel_all
        else
          ( Some last
          , cancel_all
            @ [ Submit
                  (request
                     ~instruction
                     ~quantity
                     ~order_type:Order_type.Market
                     ~time_in_force:Time_in_force.IOC)
              ] )
      | false ->
        (* Rest. The drift budget doubles as the size we are willing to have
           working passively: it is exactly the amount we can afford not to
           have traded yet. *)
        let wanted = Int.min (total - filled) drift_budget in
        let quantity = wanted - resting in
        if quantity <= 0
        then Some last, cancel_stale
        else
          ( Some last
          , cancel_stale
            @ [ Submit
                  (request
                     ~instruction
                     ~quantity
                     ~order_type:(Order_type.Limit limit)
                     ~time_in_force:Time_in_force.Day)
              ] )
    ;;
  end)
;;
