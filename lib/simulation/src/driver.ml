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
  ?engine
  ~day
  ~instructions
  ~algorithm
  ()
  =
  let engine =
    match engine with
    | Some engine -> engine
    | None -> Fill_model.engine fill_config
  in
  let (module A : Algorithm_intf.S) = algorithm in
  let bars = day.Trading_day.bars in
  let first_bar = List.hd_exn bars in
  let manager = Order_manager.create ~instructions in
  let states =
    List.map (Order_manager.parents manager) ~f:(fun parent ->
      A.init ~parent)
  in
  (* Minute zero has no previous bar, so no algorithm turn — and so no
     activation either: a parent activated here would sample its arrival
     price from a minute in which nothing it does could possibly trade, and
     every later grade would carry that unreachable price as though execution
     had missed it. Instructions arriving in the first minute activate at the
     second, where a decision is finally possible. *)
  let engine, (_ : Fill.t list) =
    Engine_intf.advance engine ~bar:first_bar ~resting_orders:[]
  in
  let step (manager, engine, states, all_fills) (previous_bar, bar) =
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
    let engine, resting_fills =
      Engine_intf.advance
        engine
        ~bar
        ~resting_orders:(live_orders_of manager)
    in
    let manager = apply_fills manager resting_fills in
    let manager, engine, action_fills =
      List.fold
        (List.map states_and_decisions ~f:snd)
        ~init:(manager, engine, [])
        ~f:(fun init (parent_index, actions) ->
          List.fold actions ~init ~f:(fun (manager, engine, fills) action ->
            match (action : Algorithm_intf.Action.t) with
            | Submit request ->
              let manager, child =
                Order_manager.submit_exn manager ~parent_index ~request ~now
              in
              let engine, new_fills = Engine_intf.child_order engine child in
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
              manager, engine, fills @ new_fills
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
                 , engine
                 , fills )
               | Some _ | None -> manager, engine, fills)))
    in
    manager, engine, states, all_fills @ resting_fills @ action_fills
  in
  let manager, (_ : Engine_intf.t), (_ : A.state list), fills =
    List.fold
      (List.zip_exn (List.drop_last_exn bars) (List.tl_exn bars))
      ~init:(manager, engine, states, [])
      ~f:step
  in
  { manager; fills }
;;
