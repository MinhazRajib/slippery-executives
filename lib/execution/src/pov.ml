open! Core
open! Execlab_types
open! Execlab_market

let create ?min_child_size ?max_child_size ~participation_rate ()
  : Algorithm_intf.t
  =
  (module struct
    type state = { observed_volume : int }

    let name = "pov"
    let init ~parent:(_ : Parent_order.t) = { observed_volume = 0 }

    let market_ioc ~(instruction : Alpha_instruction.t) ~quantity =
      Or_error.ok_exn
        (Child_order.Request.create
           ~symbol:instruction.symbol
           ~side:instruction.side
           ~quantity:(Size.of_int quantity)
           ~order_type:Order_type.Market
           ~time_in_force:Time_in_force.IOC)
    ;;

    let on_bar state (context : Algorithm_intf.Context.t) =
      let parent = context.parent in
      let instruction = parent.instruction in
      let arrival = instruction.Alpha_instruction.arrival_time in
      let deadline = instruction.Alpha_instruction.deadline in
      let bar = context.previous_bar in
      (* The bar preceding activation is pre-decision history: only bars
         at-or-after arrival are tape we chose to trade against. *)
      let observed_volume =
        if Time_ns.Ofday.( >= ) bar.Market_bar.time arrival
        then state.observed_volume + Size.to_int bar.Market_bar.volume
        else state.observed_volume
      in
      let state = { observed_volume } in
      let remaining = Size.to_int (Parent_order.remaining parent) in
      if Time_ns.Ofday.( >= ) context.now deadline
      then
        (* The deadline bar is the last that trades: demand the whole
           remainder, rate (and size knobs) be damned, so completion stays
           comparable with the schedule algorithms. *)
        if remaining <= 0
        then state, []
        else
          ( state
          , [ Algorithm_intf.Action.Submit
                (market_ioc ~instruction ~quantity:remaining)
            ] )
      else (
        (* Floor, never round: the rate is a ceiling on our tape share. *)
        let target =
          Float.iround_down_exn
            (participation_rate *. Float.of_int observed_volume)
        in
        let spoken_for =
          Size.to_int parent.filled
          + Size.to_int (Parent_order.working parent)
        in
        let deficit = Int.min (target - spoken_for) remaining in
        let deficit =
          match max_child_size with
          | None -> deficit
          | Some max_size -> Int.min deficit (Size.to_int max_size)
        in
        let is_dust =
          match min_child_size with
          | None -> false
          | Some min_size -> deficit < Size.to_int min_size
        in
        if deficit <= 0 || is_dust
        then state, []
        else
          ( state
          , [ Algorithm_intf.Action.Submit
                (market_ioc ~instruction ~quantity:deficit)
            ] ))
    ;;
  end)
;;
