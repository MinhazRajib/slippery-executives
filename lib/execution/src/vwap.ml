open! Core
open! Execlab_types

let create ~profiles : Algorithm_intf.t =
  (module struct
    type state = unit

    let name = "vwap"
    let init ~parent:(_ : Parent_order.t) = ()

    let weight ~profile ~from ~before =
      List.sum (module Float) profile ~f:(fun (time, weight) ->
        if Time_ns.Ofday.( <= ) from time && Time_ns.Ofday.( < ) time before
        then weight
        else 0.)
    ;;

    (* Twap's schedule with profile weight in place of minutes: the target
       at [now] is total * weight [arrival, now) / weight [arrival,
       deadline), rounded to nearest (which also absorbs float-summation
       noise). At [now = deadline] both sums run over the same entries, so
       the ratio is exactly 1 and the target exactly [total]. *)
    let on_bar () (context : Algorithm_intf.Context.t) =
      let parent = context.parent in
      let instruction = parent.instruction in
      let arrival = instruction.Alpha_instruction.arrival_time in
      let deadline = instruction.Alpha_instruction.deadline in
      let profile =
        Option.value
          (Map.find profiles instruction.Alpha_instruction.symbol)
          ~default:[]
      in
      let window_weight = weight ~profile ~from:arrival ~before:deadline in
      let elapsed_weight =
        weight ~profile ~from:arrival ~before:context.now
      in
      let total = Size.to_int (Parent_order.total parent) in
      let target =
        if Float.( > ) window_weight 0.
        then
          Float.iround_nearest_exn
            (Float.of_int total *. elapsed_weight /. window_weight)
        else if List.is_empty profile
        then (
          (* No forecast for this symbol: degrade to the straight line rather
             than to a market order. A schedule we cannot shape is still a
             schedule, and Twap is what shaping nothing means. *)
          let minutes between = Float.to_int (Time_ns.Span.to_min between) in
          let total_minutes =
            minutes (Time_ns.Ofday.diff deadline arrival)
          in
          let elapsed_minutes =
            minutes (Time_ns.Ofday.diff context.now arrival)
          in
          if total_minutes = 0
          then total
          else total * elapsed_minutes / total_minutes)
        else total
      in
      let spoken_for =
        Size.to_int parent.filled + Size.to_int (Parent_order.working parent)
      in
      let deficit =
        Int.min
          (target - spoken_for)
          (Size.to_int (Parent_order.remaining parent))
      in
      if deficit <= 0
      then (), []
      else (
        let request =
          Or_error.ok_exn
            (Child_order.Request.create
               ~symbol:instruction.Alpha_instruction.symbol
               ~side:instruction.Alpha_instruction.side
               ~quantity:(Size.of_int deficit)
               ~order_type:Order_type.Market
               ~time_in_force:Time_in_force.IOC)
        in
        (), [ Algorithm_intf.Action.Submit request ])
    ;;
  end)
;;
