open! Core
open! Execlab_types

let create ~urgency () : Algorithm_intf.t =
  (module struct
    type state = unit

    let name = "is"
    let init ~parent:(_ : Parent_order.t) = ()

    (* Twap's schedule with the straight line bent by Almgren-Chriss: target
       at elapsed fraction [f] is total - round(total * sinh (urgency * (1 -
       f)) / sinh urgency). The [urgency <= 0] guard is the analytic limit
       (the straight line), not a special case: sinh(ux)/sinh(u) -> x as u ->
       0, but 0/0 needs helping over. *)
    let on_bar () (context : Algorithm_intf.Context.t) =
      let parent = context.parent in
      let instruction = parent.instruction in
      let arrival = instruction.Alpha_instruction.arrival_time in
      let deadline = instruction.Alpha_instruction.deadline in
      let minutes between = Float.to_int (Time_ns.Span.to_min between) in
      let total_minutes = minutes (Time_ns.Ofday.diff deadline arrival) in
      let elapsed_minutes =
        minutes (Time_ns.Ofday.diff context.now arrival)
      in
      let total = Size.to_int (Parent_order.total parent) in
      let target =
        if total_minutes = 0
        then total
        else if Float.( <= ) urgency 1e-6
        then
          (* The zero-urgency limit is Twap, so compute it Twap's way — the
             same integer division, truncating alike — and the two agree
             share for share rather than merely in shape. Rounding the curve
             to nearest, as the general case does, would part company with
             Twap by a share whenever the ideal position lands past the half. *)
          total * elapsed_minutes / total_minutes
        else (
          let f = elapsed_minutes // total_minutes in
          (* The ratio in exponential form: for 0 <= a <= b, sinh a / sinh b
             = e^(a-b) * (1 - e^(-2a)) / (1 - e^(-2b)), which cannot overflow
             the way naive sinh does (sinh 711 = inf, and inf/inf = nan would
             blow up the rounding below). Below ~1e-6 the exponential form is
             0/0, and the ratio is the straight line to float precision
             anyway. *)
          let a = urgency *. (1. -. f) in
          let remaining_fraction =
            Float.exp (a -. urgency)
            *. (1. -. Float.exp (-2. *. a))
            /. (1. -. Float.exp (-2. *. urgency))
          in
          total
          - Float.iround_nearest_exn
              (Float.of_int total *. remaining_fraction))
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
