open! Core
open! Execlab_types

type t =
  { generator : Order_id.Generator.t
  ; parents : Parent_order.t list
  }
[@@deriving sexp_of]

let create ~instructions =
  { generator = Order_id.Generator.create ()
  ; parents = List.map instructions ~f:Parent_order.create
  }
;;

let parents t = t.parents
let parent_exn t index = List.nth_exn t.parents index

let update_parent t ~index ~f =
  { t with
    parents =
      List.mapi t.parents ~f:(fun i parent ->
        if i = index then f parent else parent)
  }
;;

(* Fills and cancels arrive with only an order id; the owning parent is found
   by searching each parent's children. *)
let find_parent_index_exn t ~order_id ~here =
  let owns (parent : Parent_order.t) =
    List.exists parent.children ~f:(fun child ->
      Order_id.equal child.Child_order.id order_id)
  in
  match List.findi t.parents ~f:(fun (_ : int) parent -> owns parent) with
  | Some (index, _) -> index
  | None ->
    raise_s
      [%message
        "Order_manager: unknown order id"
          (here : string)
          (order_id : Order_id.t)]
;;

let activate_due t ~now ~price_for =
  { t with
    parents =
      List.map t.parents ~f:(fun (parent : Parent_order.t) ->
        let instruction = parent.instruction in
        match parent.status with
        | Pending
          when Time_ns.Ofday.( <= )
                 instruction.Alpha_instruction.arrival_time
                 now ->
          Parent_order.activate_exn
            parent
            ~arrival_price:(price_for instruction.Alpha_instruction.symbol)
        | Pending | Active | Completed | Expired -> parent)
  }
;;

let submit_exn t ~parent_index ~request ~now =
  let id = Order_id.Generator.next t.generator in
  let child = Child_order.create ~request ~id ~submitted_at:now in
  let t =
    update_parent t ~index:parent_index ~f:(fun parent ->
      Parent_order.add_child_exn parent child)
  in
  t, child
;;

let cancel_exn t ~order_id ~reason =
  let index = find_parent_index_exn t ~order_id ~here:"cancel_exn" in
  update_parent t ~index ~f:(fun parent ->
    Parent_order.cancel_child_exn parent ~order_id ~reason)
;;

let apply_fill_exn t (fill : Fill.t) =
  let index =
    find_parent_index_exn t ~order_id:fill.order_id ~here:"apply_fill_exn"
  in
  update_parent t ~index ~f:(fun parent ->
    Parent_order.apply_fill_exn parent fill)
;;

let expire_due t ~now =
  { t with
    parents =
      List.map t.parents ~f:(fun (parent : Parent_order.t) ->
        let deadline = parent.instruction.Alpha_instruction.deadline in
        if Parent_order.is_active parent && Time_ns.Ofday.( > ) now deadline
        then Parent_order.expire_exn parent
        else parent)
  }
;;
