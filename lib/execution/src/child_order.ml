open! Core
open! Execlab_types

module Status = struct
  type t =
    | Live
    | Filled
    | Canceled of Cancel_reason.t
  [@@deriving sexp_of, compare, equal]
end

module Request = struct
  type t =
    { symbol : Symbol.t
    ; side : Side.t
    ; quantity : Size.t
    ; order_type : Order_type.t
    ; time_in_force : Time_in_force.t
    }
  [@@deriving sexp_of, compare, equal]

  let create ~symbol ~side ~quantity ~order_type ~time_in_force =
    if Size.( <= ) quantity Size.zero
    then
      Or_error.error_s
        [%message "Quantity must be positive" (quantity : Size.t)]
    else Ok { symbol; side; quantity; order_type; time_in_force }
  ;;
end

type t =
  { id : Order_id.t
  ; request : Request.t
  ; submitted_at : Time_ns.Ofday.t
  ; remaining : Size.t
  ; status : Status.t
  }
[@@deriving sexp_of, compare, equal]

let create ~request ~id ~submitted_at =
  { id
  ; request
  ; submitted_at
  ; remaining = request.Request.quantity
  ; status = Live
  }
;;

let require_live t ~here =
  match t.status with
  | Live -> ()
  | Filled | Canceled _ ->
    raise_s
      [%message "Child_order: order is not live" (here : string) (t : t)]
;;

let apply_fill_exn t ~quantity =
  require_live t ~here:"apply_fill_exn";
  if Size.( <= ) quantity Size.zero || Size.( > ) quantity t.remaining
  then
    raise_s
      [%message
        "Child_order.apply_fill_exn: invalid fill size"
          (quantity : Size.t)
          (t.remaining : Size.t)];
  let remaining = Size.( - ) t.remaining quantity in
  let status : Status.t =
    if Size.equal remaining Size.zero then Filled else Live
  in
  { t with remaining; status }
;;

let cancel_exn t ~reason =
  require_live t ~here:"cancel_exn";
  { t with status = Canceled reason }
;;

let is_live t =
  match t.status with Live -> true | Filled | Canceled _ -> false
;;
