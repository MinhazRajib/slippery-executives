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
  ?engine_for
  ~universe
  ~instructions
  ~algorithm
  ()
  =
  let engine_for =
    match engine_for with
    | Some engine_for -> engine_for
    | None -> fun (_ : Symbol.t) -> Fill_model.engine fill_config
  in
  let (module A : Algorithm_intf.S) = algorithm in
  (match
     List.find instructions ~f:(fun instruction ->
       not (Universe.mem universe instruction.Alpha_instruction.symbol))
   with
   | None -> ()
   | Some instruction ->
     raise_s
       [%message
         "Driver.run: instruction names a symbol this run has no session for"
           (instruction : Alpha_instruction.t)
           ~universe:(Universe.symbols universe : Symbol.t list)]);
  let manager = Order_manager.create ~instructions in
  let states =
    List.map (Order_manager.parents manager) ~f:(fun parent ->
      A.init ~parent)
  in
  (* Every symbol gets its own engine, so a bar's participation budget — or,
     in the synthetic exchange, a whole order book — belongs to one name and
     cannot be spent by trading in another. *)
  let engines =
    Symbol.Map.of_alist_exn
      (List.map (Universe.symbols universe) ~f:(fun symbol ->
         symbol, engine_for symbol))
  in
  (* Minute zero has no previous bar, so no algorithm turn — and so no
     activation either: a parent activated here would sample its arrival
     price from a minute in which nothing it does could possibly trade, and
     every later grade would carry that unreachable price as though execution
     had missed it. Instructions arriving in the first minute activate at the
     second, where a decision is finally possible. *)
  let engines =
    Map.mapi engines ~f:(fun ~key:symbol ~data:engine ->
      fst
        (Engine_intf.advance
           engine
           ~bar:(Universe.bar_exn universe ~symbol ~minute:0)
           ~resting_orders:[]))
  in
  let symbol_of (child : Child_order.t) = child.request.symbol in
  let step (manager, engines, states, all_fills) minute =
    let bar_for symbol = Universe.bar_exn universe ~symbol ~minute in
    let now = Universe.time_at universe ~minute in
    let manager =
      Order_manager.activate_due manager ~now ~price_for:(fun symbol ->
        (bar_for symbol).Market_bar.open_)
    in
    let manager = Order_manager.expire_due manager ~now in
    (* Algorithms decide from pre-bar state: the current bar's trading is
       still in their future, and each parent sees only its own symbol. *)
    let states_and_decisions =
      List.mapi (Order_manager.parents manager) ~f:(fun index parent ->
        if not (Parent_order.is_active parent)
        then List.nth_exn states index, (index, [])
        else (
          let context =
            { Algorithm_intf.Context.now
            ; previous_bar =
                Universe.bar_exn
                  universe
                  ~symbol:parent.instruction.Alpha_instruction.symbol
                  ~minute:(minute - 1)
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
    (* The bar trades: resting orders get first claim on their own symbol's
       budget. *)
    let resting = live_orders_of manager in
    let engines, resting_fills =
      Map.fold
        engines
        ~init:(Symbol.Map.empty, [])
        ~f:(fun ~key:symbol ~data:engine (engines, fills) ->
          let engine, new_fills =
            Engine_intf.advance
              engine
              ~bar:(bar_for symbol)
              ~resting_orders:
                (List.filter resting ~f:(fun child ->
                   Symbol.equal (symbol_of child) symbol))
          in
          Map.set engines ~key:symbol ~data:engine, fills @ new_fills)
    in
    let manager = apply_fills manager resting_fills in
    let manager, engines, action_fills =
      List.fold
        (List.map states_and_decisions ~f:snd)
        ~init:(manager, engines, [])
        ~f:(fun init (parent_index, actions) ->
          List.fold actions ~init ~f:(fun (manager, engines, fills) action ->
            match (action : Algorithm_intf.Action.t) with
            | Submit request ->
              let manager, child =
                Order_manager.submit_exn manager ~parent_index ~request ~now
              in
              let symbol = symbol_of child in
              let engine, new_fills =
                Engine_intf.child_order (Map.find_exn engines symbol) child
              in
              let engines = Map.set engines ~key:symbol ~data:engine in
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
              manager, engines, fills @ new_fills
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
                 , engines
                 , fills )
               | Some _ | None -> manager, engines, fills)))
    in
    manager, engines, states, all_fills @ resting_fills @ action_fills
  in
  let manager, (_ : Engine_intf.t Symbol.Map.t), (_ : A.state list), fills =
    List.fold
      (List.range 1 (Universe.minutes universe))
      ~init:(manager, engines, states, [])
      ~f:step
  in
  { manager; fills }
;;
