open! Core
open! Execlab_types
open! Execlab_market
open! Execlab_execution

type t =
  { manager : Order_manager.t
  ; fills : Fill.t list
  }
[@@deriving sexp_of]

let live_orders_of manager =
  List.concat_map
    (Order_manager.parents manager)
    ~f:Parent_order.live_children
;;

let apply_fills manager fills =
  List.fold fills ~init:manager ~f:Order_manager.apply_fill_exn
;;

let find_child manager ~parent_index ~order_id =
  List.find
    (Order_manager.parent_exn manager parent_index).Parent_order.children
    ~f:(fun child -> Order_id.equal child.Child_order.id order_id)
;;

let never_rests (request : Child_order.Request.t) =
  match request.order_type, request.time_in_force with
  | Market, (Day | IOC) -> true
  | Limit _, IOC -> true
  | Limit _, Day -> false
;;

let run
  ?(fill_config = Fill_model.Config.default)
  ~day
  ~instructions
  ~algorithm
  ()
  =
  let (module A : Algorithm_intf.S) = algorithm in
  let bars = day.Trading_day.bars in
  let first_bar = List.hd_exn bars in
  let manager = Order_manager.create ~instructions in
  let states =
    List.map (Order_manager.parents manager) ~f:(fun parent ->
      A.init ~parent)
  in
  (* Minute zero has no previous bar, so no algorithm turn: just activate and
     show the fill model its first bar. *)
  let manager =
    Order_manager.activate_due
      manager
      ~now:first_bar.Market_bar.time
      ~price_for:(fun (_ : Symbol.t) -> first_bar.Market_bar.open_)
  in
  let fill_model, (_ : Fill.t list) =
    Fill_model.on_bar_advance
      (Fill_model.create fill_config)
      ~bar:first_bar
      ~resting_orders:[]
  in
  let step (manager, fill_model, states, all_fills) (previous_bar, bar) =
    let now = bar.Market_bar.time in
    let manager =
      Order_manager.activate_due
        manager
        ~now
        ~price_for:(fun (_ : Symbol.t) -> bar.Market_bar.open_)
    in
    let manager = Order_manager.expire_due manager ~now in
    (* Algorithms decide from pre-bar state: the current bar's trading is
       still in their future. *)
    let states_and_decisions =
      List.mapi (Order_manager.parents manager) ~f:(fun index parent ->
        if not (Parent_order.is_active parent)
        then List.nth_exn states index, (index, [])
        else (
          let context =
            { Algorithm_intf.Context.now
            ; previous_bar
            ; parent
            ; live_orders = Parent_order.live_children parent
            }
          in
          let state, actions =
            A.on_bar (List.nth_exn states index) context
          in
          state, (index, actions)))
    in
    let states = List.map states_and_decisions ~f:fst in
    (* The bar trades: resting orders get first claim on the budget. *)
    let fill_model, resting_fills =
      Fill_model.on_bar_advance
        fill_model
        ~bar
        ~resting_orders:(live_orders_of manager)
    in
    let manager = apply_fills manager resting_fills in
    let manager, fill_model, action_fills =
      List.fold
        (List.map states_and_decisions ~f:snd)
        ~init:(manager, fill_model, [])
        ~f:(fun init (parent_index, actions) ->
          List.fold
            actions
            ~init
            ~f:(fun (manager, fill_model, fills) action ->
              match (action : Algorithm_intf.Action.t) with
              | Submit request ->
                let manager, child =
                  Order_manager.submit_exn
                    manager
                    ~parent_index
                    ~request
                    ~now
                in
                let fill_model, new_fills =
                  Fill_model.on_child_order fill_model child
                in
                let manager = apply_fills manager new_fills in
                let manager =
                  if not (never_rests child.request)
                  then manager
                  else (
                    match
                      find_child manager ~parent_index ~order_id:child.id
                    with
                    | Some child when Child_order.is_live child ->
                      Order_manager.cancel_exn
                        manager
                        ~order_id:child.id
                        ~reason:Ioc_remainder
                    | Some _ | None -> manager)
                in
                manager, fill_model, fills @ new_fills
              | Cancel order_id ->
                (* The order may have died this very bar; canceling a dead
                   order is the race resolving in the market's favor, not a
                   bug. *)
                (match find_child manager ~parent_index ~order_id with
                 | Some child when Child_order.is_live child ->
                   ( Order_manager.cancel_exn
                       manager
                       ~order_id
                       ~reason:Algorithm_requested
                   , fill_model
                   , fills )
                 | Some _ | None -> manager, fill_model, fills)))
    in
    manager, fill_model, states, all_fills @ resting_fills @ action_fills
  in
  let manager, (_ : Fill_model.t), (_ : A.state list), fills =
    List.fold
      (List.zip_exn (List.drop_last_exn bars) (List.tl_exn bars))
      ~init:(manager, fill_model, states, [])
      ~f:step
  in
  { manager; fills }
;;
