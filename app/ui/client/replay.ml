(* Reconstructing the session timeline from a finished run. The simulation is
   deterministic, so "live playback" is just a prefix view of the batch
   result: everything on the Simulate screen — event log, parent-order panel,
   child-order blotter — is a pure function of ({!Sim.Output.t}, current
   minute). Scrubbing backwards is recomputing a shorter prefix. *)

open! Core
open! Execlab_types
open! Execlab_execution

let session_open = Time_ns.Ofday.create ~hr:9 ~min:30 ()
let last_minute = 389

let minute_of_ofday ofday =
  Float.to_int (Time_ns.Span.to_min (Time_ns.Ofday.diff ofday session_open))
;;

module Event = struct
  module Kind = struct
    type t =
      | Activated
      | Fill_buy
      | Fill_sell
      | Canceled
      | Expired
      | Completed
    [@@deriving sexp_of, compare, equal]

    (* Matches the [.ev-*] classes in [index.html]. *)
    let css_class = function
      | Activated -> "ev-activate"
      | Fill_buy -> "ev-fill-buy"
      | Fill_sell -> "ev-fill-sell"
      | Canceled -> "ev-cancel"
      | Expired -> "ev-expire"
      | Completed -> "ev-complete"
    ;;
  end

  type t =
    { minute : int
    ; kind : Kind.t
    ; message : string
    }
end

(* The minute a canceled child left the market. IOC remainders die in their
   submission bar; deadline expiry happens the minute after the deadline (the
   deadline minute itself may still trade); the others are best-effort. *)
let cancel_minute (child : Child_order.t) ~deadline =
  match child.status with
  | Child_order.Status.Canceled Cancel_reason.Deadline_expired ->
    Int.min last_minute (minute_of_ofday deadline + 1)
  | Child_order.Status.Canceled Cancel_reason.End_of_day -> last_minute
  | Child_order.Status.Canceled
      ( Cancel_reason.Ioc_remainder | Cancel_reason.Passive_timeout
      | Cancel_reason.Algorithm_requested )
  | Child_order.Status.Live | Child_order.Status.Filled ->
    minute_of_ofday child.submitted_at
;;

(* Shares of [fills] executed at or before [minute]. *)
let filled_by (fills : Fill.t list) ~minute =
  List.sum (module Int) fills ~f:(fun fill ->
    if minute_of_ofday fill.time <= minute then Size.to_int fill.size else 0)
;;

(* The minute an instruction's cumulative fills first reached its full
   quantity, if they ever did. *)
let completion_minute (fills : Fill.t list) ~quantity =
  let sorted =
    List.sort fills ~compare:(fun a b ->
      Time_ns.Ofday.compare a.Fill.time b.Fill.time)
  in
  With_return.with_return (fun { return } ->
    let (_ : int) =
      List.fold sorted ~init:0 ~f:(fun so_far (fill : Fill.t) ->
        let so_far = so_far + Size.to_int fill.size in
        if so_far >= quantity then return (Some (minute_of_ofday fill.time));
        so_far)
    in
    None)
;;

let events (output : Sim.Output.t) : Event.t list =
  let instruction_events =
    List.concat_mapi output.graded ~f:(fun index (graded : Sim.Graded.t) ->
      let instruction = graded.instruction in
      let label =
        [%string
          "#%{index + 1#Int} %{Side.to_string instruction.side} \
           %{Fmt.shares instruction.quantity}"]
      in
      let fills =
        Option.value (Map.find output.fills_by_parent index) ~default:[]
      in
      let quantity = Size.to_int instruction.quantity in
      let activated =
        { Event.minute = minute_of_ofday instruction.arrival_time
        ; kind = Event.Kind.Activated
        ; message =
            [%string
              "%{label} activated · arrival %{Fmt.price \
               graded.algo.arrival_price} · deadline %{Fmt.ofday \
               instruction.deadline}"]
        }
      in
      let completed =
        match completion_minute fills ~quantity with
        | None -> []
        | Some minute ->
          [ { Event.minute
            ; kind = Event.Kind.Completed
            ; message = [%string "%{label} complete"]
            }
          ]
      in
      let expired =
        let filled =
          List.sum (module Int) fills ~f:(fun f -> Size.to_int f.size)
        in
        if filled >= quantity
        then []
        else
          [ { Event.minute =
                Int.min last_minute (minute_of_ofday instruction.deadline + 1)
            ; kind = Event.Kind.Expired
            ; message =
                [%string
                  "%{label} expired · %{Fmt.shares_int (quantity - filled)} \
                   unfilled"]
            }
          ]
      in
      (activated :: completed) @ expired)
  in
  let fill_events =
    List.map output.fills ~f:(fun (fill : Fill.t) ->
      let kind =
        match fill.side with
        | Side.Buy -> Event.Kind.Fill_buy
        | Side.Sell -> Event.Kind.Fill_sell
      in
      { Event.minute = minute_of_ofday fill.time
      ; kind
      ; message =
          [%string
            "%{Side.to_string fill.side} %{Fmt.shares fill.size} @ \
             %{Fmt.price fill.price} (order %{Order_id.to_string \
             fill.order_id})"]
      })
  in
  let cancel_events =
    List.concat_mapi output.parents ~f:(fun index parent ->
      let deadline = parent.instruction.Alpha_instruction.deadline in
      List.filter_map parent.children ~f:(fun child ->
        match child.status with
        | Child_order.Status.Live | Child_order.Status.Filled -> None
        | Child_order.Status.Canceled reason ->
          let remaining = Size.to_int child.remaining in
          if remaining = 0
          then None
          else
            Some
              { Event.minute = cancel_minute child ~deadline
              ; kind = Event.Kind.Canceled
              ; message =
                  [%string
                    "order %{Order_id.to_string child.id} canceled \
                     (%{Cancel_reason.to_string reason}) · #%{index + \
                     1#Int} · %{Fmt.shares_int remaining} unfilled"]
              }))
  in
  List.stable_sort
    (instruction_events @ fill_events @ cancel_events)
    ~compare:(fun a b -> Int.compare a.Event.minute b.Event.minute)
;;

module Parent_view = struct
  (* One instruction's live panel row at a playback minute. *)
  module Status = struct
    type t =
      | Pending
      | Active
      | Completed
      | Expired

    let label = function
      | Pending -> "pending"
      | Active -> "working"
      | Completed -> "complete"
      | Expired -> "expired"
    ;;

    (* Matches the [.status-pill] modifier classes. *)
    let css_class = function
      | Pending -> ""
      | Active -> "active"
      | Completed -> "completed"
      | Expired -> "expired"
    ;;
  end

  type t =
    { instruction : Alpha_instruction.t
    ; status : Status.t
    ; filled : int
    ; quantity : int
    ; average_price : float option (* size-weighted, over fills so far *)
    }

  let at (output : Sim.Output.t) ~minute : t list =
    List.mapi output.graded ~f:(fun index (graded : Sim.Graded.t) ->
      let instruction = graded.instruction in
      let quantity = Size.to_int instruction.quantity in
      let fills =
        Option.value (Map.find output.fills_by_parent index) ~default:[]
      in
      let filled = filled_by fills ~minute in
      let status : Status.t =
        if minute < minute_of_ofday instruction.arrival_time
        then Pending
        else if filled >= quantity
        then (
          match completion_minute fills ~quantity with
          | Some done_at when done_at <= minute -> Completed
          | Some (_ : int) | None -> Active)
        else if minute > minute_of_ofday instruction.deadline
        then Expired
        else Active
      in
      let average_price =
        let notional, shares =
          List.fold fills ~init:(0., 0) ~f:(fun (notional, shares) fill ->
            if minute_of_ofday fill.Fill.time <= minute
            then
              ( notional
                +. (Price.to_float fill.price *. Size.to_float fill.size)
              , shares + Size.to_int fill.size )
            else notional, shares)
        in
        if shares = 0 then None else Some (notional /. Float.of_int shares)
      in
      { instruction; status; filled; quantity; average_price })
  ;;
end

module Blotter_row = struct
  (* One child order in the blotter at a playback minute. *)
  module Status = struct
    type t =
      | Live
      | Filled
      | Canceled of Cancel_reason.t

    let label = function
      | Live -> "live"
      | Filled -> "filled"
      | Canceled reason -> Cancel_reason.to_string reason
    ;;
  end

  type t =
    { id : Order_id.t
    ; submitted_minute : int
    ; side : Side.t
    ; order_type : Order_type.t
    ; quantity : int
    ; filled : int
    ; status : Status.t
    }

  (* Children already submitted at [minute], newest first. *)
  let at (output : Sim.Output.t) ~minute : t list =
    let fills_by_order =
      List.fold output.fills ~init:Order_id.Map.empty ~f:(fun map fill ->
        Map.add_multi map ~key:fill.Fill.order_id ~data:fill)
    in
    List.concat_map output.parents ~f:(fun parent ->
      let deadline = parent.instruction.Alpha_instruction.deadline in
      List.filter_map parent.children ~f:(fun child ->
        let submitted_minute = minute_of_ofday child.submitted_at in
        if submitted_minute > minute
        then None
        else (
          let fills =
            Option.value (Map.find fills_by_order child.id) ~default:[]
          in
          let filled = filled_by fills ~minute in
          let quantity = Size.to_int child.request.quantity in
          let status : Status.t =
            if filled >= quantity
            then Filled
            else (
              match child.status with
              | Child_order.Status.Canceled reason
                when cancel_minute child ~deadline <= minute ->
                Canceled reason
              | Child_order.Status.Canceled (_ : Cancel_reason.t)
              | Child_order.Status.Live | Child_order.Status.Filled ->
                Live)
          in
          Some
            { id = child.id
            ; submitted_minute
            ; side = child.request.side
            ; order_type = child.request.order_type
            ; quantity
            ; filled
            ; status
            })))
    |> List.sort ~compare:(fun a b ->
      match Int.compare b.submitted_minute a.submitted_minute with
      | 0 -> Order_id.compare b.id a.id
      | order -> order)
  ;;
end
