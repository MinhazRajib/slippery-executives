open! Core
open! Execlab_types

module Status = struct
  type t =
    | Pending
    | Active
    | Completed
    | Expired
  [@@deriving sexp_of, compare, equal]
end

type t =
  { instruction : Alpha_instruction.t
  ; status : Status.t
  ; arrival_price : Price.t option
  ; filled : Size.t
  ; children : Child_order.t list
  }
[@@deriving sexp_of, compare, equal]

let total t = t.instruction.Alpha_instruction.quantity

let working t =
  List.filter t.children ~f:Child_order.is_live
  |> List.sum (module Size) ~f:(fun child -> child.remaining)
;;

let remaining t = Size.( - ) (Size.( - ) (total t) t.filled) (working t)

let require_status t ~expected ~here =
  if not (Status.equal t.status expected)
  then
    raise_s
      [%message
        "Parent_order: unexpected status"
          (here : string)
          (expected : Status.t)
          ~actual:(t.status : Status.t)]
;;

let create instruction =
  { instruction
  ; status = Pending
  ; arrival_price = None
  ; filled = Size.zero
  ; children = []
  }
;;

let live_children t = List.filter t.children ~f:Child_order.is_live
let is_active t = Status.equal t.status Active

let activate_exn t ~arrival_price =
  require_status t ~expected:Pending ~here:"activate_exn";
  { t with status = Active; arrival_price = Some arrival_price }
;;

let add_child_exn t (child : Child_order.t) =
  require_status t ~expected:Active ~here:"add_child_exn";
  let instruction_symbol = t.instruction.Alpha_instruction.symbol in
  let child_symbol = child.request.symbol in
  if not (Symbol.equal child_symbol instruction_symbol)
  then
    raise_s
      [%message
        "Parent_order.add_child_exn: child symbol does not match instruction"
          (child_symbol : Symbol.t)
          (instruction_symbol : Symbol.t)];
  let child_quantity = child.request.quantity in
  let filled = t.filled in
  let working = working t in
  let total = total t in
  if Size.( > ) (Size.( + ) (Size.( + ) filled working) child_quantity) total
  then
    raise_s
      [%message
        "Parent_order.add_child_exn: would exceed parent quantity"
          (child_quantity : Size.t)
          (filled : Size.t)
          (working : Size.t)
          (total : Size.t)];
  { t with children = child :: t.children }
;;

let apply_fill_exn t (fill : Fill.t) =
  require_status t ~expected:Active ~here:"apply_fill_exn";
  if not
       (List.exists t.children ~f:(fun child ->
          Order_id.equal child.Child_order.id fill.order_id))
  then
    raise_s
      [%message
        "Parent_order.apply_fill_exn: unknown order id" (fill : Fill.t)];
  let children =
    List.map t.children ~f:(fun child ->
      if Order_id.equal child.Child_order.id fill.order_id
      then Child_order.apply_fill_exn child ~quantity:fill.size
      else child)
  in
  let filled = Size.( + ) t.filled fill.size in
  let status : Status.t =
    if Size.equal filled (total t) then Completed else t.status
  in
  { t with children; filled; status }
;;

let expire_exn t =
  require_status t ~expected:Active ~here:"expire_exn";
  let children =
    List.map t.children ~f:(fun child ->
      if Child_order.is_live child
      then Child_order.cancel_exn child ~reason:Deadline_expired
      else child)
  in
  { t with status = Expired; children }
;;
