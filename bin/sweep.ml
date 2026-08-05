(** Runs one alpha across every session, algorithm, and engine, checking the
    invariants that must hold for {e any} run and printing a league table of
    what survived execution.

    [dune exec bin/sweep.exe] checks the built-in scenarios over the whole
    catalog; [dune exec bin/sweep.exe -- TSLA] narrows it to one symbol. Any
    invariant violation is printed and makes the run exit non-zero, so this
    doubles as a regression net over the real data that the expect tests
    (sandboxed away from [data/]) cannot reach. *)

open! Core
open Execlab_types
open Execlab_market
open Execlab_execution
open! Execlab_analytics
open Execlab_simulation
open Execlab_session

let data_dir = "data"

module Scenario = struct
  type t =
    { name : string
    ; instructions : symbol:Symbol.t -> Alpha_instruction.t list
    }

  let instruction ~symbol ~side ~quantity ~arrival ~deadline =
    Or_error.ok_exn
      (Alpha_instruction.create
         ~arrival_time:(Time_ns.Ofday.of_string arrival)
         ~symbol
         ~side
         ~quantity:(Size.of_int quantity)
         ~deadline:(Time_ns.Ofday.of_string deadline))
  ;;

  let one ~name ~side ~quantity ~arrival ~deadline =
    { name
    ; instructions =
        (fun ~symbol ->
          [ instruction ~symbol ~side ~quantity ~arrival ~deadline ])
    }
  ;;

  let all =
    [ one
        ~name:"buy mid-session"
        ~side:Side.Buy
        ~quantity:5_000
        ~arrival:"10:00:00"
        ~deadline:"11:00:00"
    ; one
        ~name:"sell mid-session"
        ~side:Sell
        ~quantity:3_000
        ~arrival:"11:30:00"
        ~deadline:"13:00:00"
    ; one
        ~name:"buy at the open"
        ~side:Buy
        ~quantity:2_000
        ~arrival:"09:30:00"
        ~deadline:"10:00:00"
    ; one
        ~name:"buy into the close"
        ~side:Buy
        ~quantity:2_000
        ~arrival:"15:30:00"
        ~deadline:"15:59:00"
    ; one
        ~name:"single share"
        ~side:Buy
        ~quantity:1
        ~arrival:"12:00:00"
        ~deadline:"12:30:00"
    ; one
        ~name:"zero-length window"
        ~side:Buy
        ~quantity:1_000
        ~arrival:"12:00:00"
        ~deadline:"12:00:00"
    ; one
        ~name:"one-minute window"
        ~side:Sell
        ~quantity:1_000
        ~arrival:"12:00:00"
        ~deadline:"12:01:00"
    ; one
        ~name:"impossible size"
        ~side:Buy
        ~quantity:5_000_000
        ~arrival:"10:00:00"
        ~deadline:"10:05:00"
    ; { name = "three overlapping orders"
      ; instructions =
          (fun ~symbol ->
            [ instruction
                ~symbol
                ~side:Side.Buy
                ~quantity:4_000
                ~arrival:"10:00:00"
                ~deadline:"11:30:00"
            ; instruction
                ~symbol
                ~side:Sell
                ~quantity:2_500
                ~arrival:"10:30:00"
                ~deadline:"12:00:00"
            ; instruction
                ~symbol
                ~side:Buy
                ~quantity:1_500
                ~arrival:"11:00:00"
                ~deadline:"11:15:00"
            ])
      }
    ]
  ;;
end

let algorithms = [ "twap"; "vwap"; "pov"; "is"; "immediate" ]

let engines =
  [ "bar", Engine_choice.Bar_model
  ; "synthetic", Engine_choice.Synthetic { seed = 1 }
  ]
;;

let violations = ref 0

let check ~where ~name condition ~detail =
  if not condition
  then (
    Int.incr violations;
    printf "VIOLATION [%s] %s: %s\n" where name (force detail))
;;

(* Everything that must hold of a single graded instruction, whichever
   algorithm and engine produced it. *)
(* Every benchmark a grade is measured against must come from the order's own
   session. Checking the graded symbol is not enough: a run that graded every
   order against the first session would still report the right symbol while
   comparing TSLA's fills to AAPL's VWAP — which is precisely the way a
   multi-symbol run fails silently. *)
let check_benchmarks
  ~where
  ~universe
  ~(instruction : Alpha_instruction.t)
  ~graded
  =
  let grading = graded.Graded.grading in
  let day = Universe.day_exn universe instruction.symbol in
  check
    ~where
    ~name:"graded against its own symbol"
    (Symbol.equal grading.symbol instruction.symbol)
    ~detail:
      (lazy
        (sprintf
           !"graded %{Symbol}, instruction says %{Symbol}"
           grading.symbol
           instruction.symbol));
  check
    ~where
    ~name:"day vwap is this symbol's own"
    (Float.equal grading.day_vwap (Day_stats.vwap day))
    ~detail:
      (lazy
        (sprintf
           !"graded against %.4f; %{Symbol} traded at %.4f"
           grading.day_vwap
           instruction.symbol
           (Day_stats.vwap day)));
  check
    ~where
    ~name:"terminal price is this symbol's own close"
    (Price.equal grading.terminal_price (Benchmarks.terminal_price day))
    ~detail:
      (lazy
        (sprintf
           !"graded against %{Price}; %{Symbol} closed at %{Price}"
           grading.terminal_price
           instruction.symbol
           (Benchmarks.terminal_price day)))
;;

let check_grading ~where ~(instruction : Alpha_instruction.t) ~fills ~graded =
  let grading = graded.Graded.grading in
  let filled = Size.to_int grading.filled in
  let quantity = Size.to_int grading.quantity in
  check
    ~where
    ~name:"filled within the instruction"
    (filled <= quantity && filled >= 0)
    ~detail:(lazy (sprintf "filled %d of %d" filled quantity));
  check
    ~where
    ~name:"fills sum to the filled quantity"
    (List.sum (module Int) fills ~f:(fun (f : Fill.t) -> Size.to_int f.size)
     = filled)
    ~detail:(lazy "fill sizes disagree with the parent's filled total");
  check
    ~where
    ~name:"pnl identity"
    (grading.gross_theoretical_pnl_cents
     = grading.net_pnl_cents
       + grading.friction_cost_cents
       + grading.opportunity_cost_cents)
    ~detail:
      (lazy
        (sprintf
           "gross %d <> net %d + friction %d + opportunity %d"
           grading.gross_theoretical_pnl_cents
           grading.net_pnl_cents
           grading.friction_cost_cents
           grading.opportunity_cost_cents));
  check
    ~where
    ~name:"cost decomposition"
    (grading.friction_cost_cents
     = grading.timing_cost_cents
       + grading.spread_cost_cents
       + grading.impact_cost_cents)
    ~detail:
      (lazy
        (sprintf
           "friction %d <> timing %d + spread %d + impact %d"
           grading.friction_cost_cents
           grading.timing_cost_cents
           grading.spread_cost_cents
           grading.impact_cost_cents));
  check
    ~where
    ~name:"completion rate"
    (Float.( >= ) grading.completion_rate 0.
     && Float.( <= ) grading.completion_rate 1.0001)
    ~detail:(lazy (sprintf "completion %.4f" grading.completion_rate));
  List.iter fills ~f:(fun (fill : Fill.t) ->
    check
      ~where
      ~name:"fill is positive"
      (Size.to_int fill.size > 0 && Price.to_int_cents fill.price > 0)
      ~detail:(lazy (sprintf !"%{sexp:Fill.t}" fill));
    check
      ~where
      ~name:"fill matches the instruction"
      (Symbol.equal fill.symbol instruction.symbol
       && Side.equal fill.side instruction.side)
      ~detail:(lazy (sprintf !"%{sexp:Fill.t}" fill));
    check
      ~where
      ~name:"fill inside the execution window"
      (Time_ns.Ofday.( >= ) fill.time instruction.arrival_time
       && Time_ns.Ofday.( <= ) fill.time instruction.deadline)
      ~detail:
        (lazy
          (sprintf
             !"filled %{Time_ns.Ofday} outside \
               %{Time_ns.Ofday}-%{Time_ns.Ofday}"
             fill.time
             instruction.arrival_time
             instruction.deadline)))
;;

(* Buying cannot be cheaper than the price you decided at when the whole
   order trades in the arrival minute, and selling cannot be dearer. *)
let check_immediate ~where ~(graded : Graded.t) =
  match graded.grading.fill_metrics with
  | None -> ()
  | Some metrics ->
    let arrival = Price.to_float graded.grading.arrival_price in
    let average = metrics.average_fill_price in
    let ok =
      match graded.grading.side with
      | Side.Buy -> Float.( >= ) average (arrival -. 0.0001)
      | Sell -> Float.( <= ) average (arrival +. 0.0001)
    in
    check
      ~where
      ~name:"immediate never beats its own arrival price"
      ok
      ~detail:
        (lazy
          (sprintf
             !"%{sexp:Side.t} average %.4f vs arrival %.4f"
             graded.grading.side
             average
             arrival))
;;

module Key = struct
  module T = struct
    type t = Symbol.t * Time_ns.Ofday.t [@@deriving compare, sexp_of]
  end

  include T
  include Comparator.Make (T)
end

(* Engine A's participation cap is a per-bar budget shared by every order of
   {e one} symbol, so no minute may trade more than its share of that
   symbol's bar volume. Two names trading the same minute do not compete for
   a single budget — which is what bucketing on (symbol, time) checks. *)
let check_participation ~where ~universe ~fills ~params =
  let volume_by_time =
    List.concat_map (Universe.days universe) ~f:(fun (day : Trading_day.t) ->
      List.map day.bars ~f:(fun bar ->
        (day.symbol, bar.Market_bar.time), Size.to_int bar.volume))
    |> Map.of_alist_exn (module Key)
  in
  List.map fills ~f:(fun (fill : Fill.t) ->
    (fill.symbol, fill.time), Size.to_int fill.size)
  |> Map.of_alist_fold (module Key) ~init:0 ~f:( + )
  |> Map.iteri ~f:(fun ~key:(symbol, time) ~data:filled ->
    match Map.find volume_by_time (symbol, time) with
    | None -> ()
    | Some volume ->
      let budget =
        Float.to_int
          (params.Params.fill_config.Fill_model.Config.max_participation
           *. Float.of_int volume)
      in
      check
        ~where
        ~name:"participation cap"
        (filled <= budget)
        ~detail:
          (lazy
            (sprintf
               !"%{Symbol} %{Time_ns.Ofday}: filled %d of a %d budget"
               symbol
               time
               filled
               budget)))
;;

let run_one ~universe ~forecast_days ~instructions ~algo_name ~params =
  Execlab_session.run
    ~universe
    ~forecast_days
    ~instructions
    ~algo_name
    ~params
;;

let fills_of ~(result : Driver.t) ~(parent : Parent_order.t) =
  let ids =
    Order_id.Set.of_list
      (List.map parent.children ~f:(fun child -> child.Child_order.id))
  in
  List.filter result.fills ~f:(fun fill -> Set.mem ids fill.Fill.order_id)
;;

(* One (symbol, date, scenario, engine, algorithm) cell: run it, check
   everything that must hold, and return its league-table row. *)
let check_cell
  ~universe
  ~forecast_days
  ~scenario
  ~instructions
  ~engine_name
  ~engine
  ~algo_name
  =
  let params = { Params.default with engine } in
  let where =
    sprintf
      !"%s %{Date} %s %s/%s"
      (String.concat
         ~sep:","
         (List.map (Universe.symbols universe) ~f:Symbol.to_string))
      (Universe.date universe)
      scenario
      algo_name
      engine_name
  in
  match
    run_one ~universe ~forecast_days ~instructions ~algo_name ~params
  with
  | Error error ->
    Int.incr violations;
    printf !"VIOLATION [run] %s: %{Error#hum}\n" where error;
    None
  | Ok outcome ->
    let parents = Order_manager.parents outcome.algo_result.manager in
    List.iter2_exn parents outcome.graded ~f:(fun parent graded ->
      let fills = fills_of ~result:outcome.algo_result ~parent in
      check_grading ~where ~instruction:parent.instruction ~fills ~graded;
      check_benchmarks
        ~where
        ~universe
        ~instruction:parent.instruction
        ~graded;
      if String.equal algo_name "immediate"
      then check_immediate ~where ~graded);
    (match engine with
     | Engine_choice.Bar_model ->
       check_participation
         ~where
         ~universe
         ~fills:outcome.algo_result.fills
         ~params
     | Synthetic { seed = (_ : int) } -> ());
    Some
      ( scenario
      , algo_name
      , engine_name
      , Outcome.value_add_cents outcome
      , Outcome.shortfall_cents outcome )
;;

let check_day ~day ~forecast_days =
  let universe = Universe.of_day day in
  let forecast_days =
    Symbol.Map.singleton day.Trading_day.symbol forecast_days
  in
  List.concat_map Scenario.all ~f:(fun (scenario : Scenario.t) ->
    let instructions =
      scenario.instructions ~symbol:day.Trading_day.symbol
    in
    List.concat_map engines ~f:(fun (engine_name, engine) ->
      List.filter_map algorithms ~f:(fun algo_name ->
        check_cell
          ~universe
          ~forecast_days
          ~scenario:scenario.name
          ~instructions
          ~engine_name
          ~engine
          ~algo_name)))
;;

let sweep ~symbols =
  List.concat_map symbols ~f:(fun symbol ->
    List.concat_map
      (Execlab_server.Catalog.dates_for ~data_dir ~symbol)
      ~f:(fun date ->
        match Execlab_server.Catalog.load ~data_dir ~symbol ~date with
        | Error error ->
          Int.incr violations;
          printf
            !"VIOLATION [load] %{Symbol} %{Date}: %{Error#hum}\n"
            symbol
            date
            error;
          []
        | Ok day ->
          let forecast_days =
            Execlab_server.Catalog.forecast_days
              ~data_dir
              ~symbol
              ~excluding:date
          in
          check_day ~day ~forecast_days))
;;

(* A basket alpha: one instruction per name, staggered so their windows
   overlap, which is the case where a shared budget or a shared engine would
   show up. *)
let basket_instructions symbols =
  List.mapi symbols ~f:(fun index symbol ->
    let side = if index % 2 = 0 then Side.Buy else Side.Sell in
    let arrival = sprintf "10:%02d:00" (index * 10) in
    let deadline = sprintf "11:%02d:00" (index * 10) in
    Scenario.instruction
      ~symbol
      ~side
      ~quantity:(4_000 + (index * 1_000))
      ~arrival
      ~deadline)
;;

(* The invariant that makes a basket one run rather than several: what a
   symbol trades inside the basket must be exactly what it trades alone.
   Anything shared by accident across names — an engine, a random stream, a
   participation budget, a volume forecast — breaks this. *)
let fill_shape (outcome : Outcome.t) ~symbol =
  List.filter outcome.algo_result.fills ~f:(fun (fill : Fill.t) ->
    Symbol.equal fill.symbol symbol)
  |> List.map ~f:(fun (fill : Fill.t) ->
    ( fill.time
    , Size.to_int fill.size
    , Price.to_int_cents fill.price
    , fill.liquidity ))
;;

let check_basket ~date ~symbols ~forecast_days =
  match
    List.map symbols ~f:(fun symbol -> Map.find_exn forecast_days symbol)
  with
  | (_ : Trading_day.t list list) ->
    let days =
      List.map symbols ~f:(fun symbol ->
        Or_error.ok_exn (Execlab_server.Catalog.load ~data_dir ~symbol ~date))
    in
    let universe = Or_error.ok_exn (Universe.of_days days) in
    let instructions = basket_instructions symbols in
    List.concat_map engines ~f:(fun (engine_name, engine) ->
      List.filter_map algorithms ~f:(fun algo_name ->
        let row =
          check_cell
            ~universe
            ~forecast_days
            ~scenario:"basket"
            ~instructions
            ~engine_name
            ~engine
            ~algo_name
        in
        let params = { Params.default with engine } in
        (match
           run_one ~universe ~forecast_days ~instructions ~algo_name ~params
         with
         | Error (_ : Error.t) -> ()
         | Ok basket ->
           List.iter2_exn symbols days ~f:(fun symbol day ->
             let solo =
               run_one
                 ~universe:(Universe.of_day day)
                 ~forecast_days:
                   (Symbol.Map.singleton
                      symbol
                      (Map.find_exn forecast_days symbol))
                 ~instructions:
                   (List.filter instructions ~f:(fun instruction ->
                      Symbol.equal
                        instruction.Alpha_instruction.symbol
                        symbol))
                 ~algo_name
                 ~params
             in
             match solo with
             | Error error ->
               check
                 ~where:
                   (sprintf
                      !"%{Symbol} %{Date} basket/%s"
                      symbol
                      date
                      algo_name)
                 ~name:"solo run of a basket leg"
                 false
                 ~detail:(lazy (Error.to_string_hum error))
             | Ok solo ->
               check
                 ~where:
                   (sprintf
                      !"%{Symbol} %{Date} basket/%s/%s"
                      symbol
                      date
                      algo_name
                      engine_name)
                 ~name:"a basket leg trades exactly as it would alone"
                 ([%equal: (Time_ns.Ofday.t * int * int * Liquidity.t) list]
                    (fill_shape basket ~symbol)
                    (fill_shape solo ~symbol))
                 ~detail:
                   (lazy
                     (sprintf
                        !"basket %{sexp:(Time_ns.Ofday.t * int * int * \
                          Liquidity.t) list} vs solo \
                          %{sexp:(Time_ns.Ofday.t * int * int * \
                          Liquidity.t) list}"
                        (fill_shape basket ~symbol)
                        (fill_shape solo ~symbol)))));
        row))
;;

(* Every date where enough names traded to make a basket, checked as one run
   and against its own legs. *)
let sweep_baskets ~symbols =
  let dates =
    List.concat_map symbols ~f:(fun symbol ->
      Execlab_server.Catalog.dates_for ~data_dir ~symbol)
    |> List.dedup_and_sort ~compare:Date.compare
  in
  List.concat_map dates ~f:(fun date ->
    let trading =
      List.filter symbols ~f:(fun symbol ->
        List.mem
          (Execlab_server.Catalog.dates_for ~data_dir ~symbol)
          date
          ~equal:Date.equal)
    in
    match List.take trading 3 with
    | [] | [ _ ] -> []
    | symbols ->
      let forecast_days =
        Execlab_server.Catalog.forecast_days_for ~data_dir ~date ~symbols
      in
      check_basket ~date ~symbols ~forecast_days)
;;

(* Two runs that must agree exactly: implementation shortfall at zero urgency
   is TWAP's straight line, so the fills should be identical. *)
let check_is_reduces_to_twap ~symbols =
  List.iter symbols ~f:(fun symbol ->
    match Execlab_server.Catalog.dates_for ~data_dir ~symbol with
    | [] -> ()
    | date :: (_ : Date.t list) ->
      let day =
        Or_error.ok_exn (Execlab_server.Catalog.load ~data_dir ~symbol ~date)
      in
      let instructions =
        (List.hd_exn Scenario.all).Scenario.instructions ~symbol
      in
      let run ~algo_name ~params =
        Or_error.ok_exn
          (run_one
             ~universe:(Universe.of_day day)
             ~forecast_days:Symbol.Map.empty
             ~instructions
             ~algo_name
             ~params)
      in
      let twap = run ~algo_name:"twap" ~params:Params.default in
      let is =
        run ~algo_name:"is" ~params:{ Params.default with is_urgency = 0. }
      in
      let prices (outcome : Outcome.t) =
        List.map outcome.algo_result.fills ~f:(fun (fill : Fill.t) ->
          Size.to_int fill.size, Price.to_int_cents fill.price)
      in
      check
        ~where:(sprintf !"%{Symbol} %{Date}" symbol date)
        ~name:"zero urgency is exactly twap"
        ([%equal: (int * int) list] (prices twap) (prices is))
        ~detail:(lazy "is at urgency 0 diverged from twap"))
;;

let () =
  let symbols =
    match Sys.get_argv () with
    | [| _ |] -> Execlab_server.Catalog.symbols ~data_dir
    | [| _; symbol |] -> [ Symbol.of_string (String.uppercase symbol) ]
    | _ ->
      eprintf "usage: sweep.exe [SYMBOL]\n";
      exit 2
  in
  let rows = sweep ~symbols @ sweep_baskets ~symbols in
  check_is_reduces_to_twap ~symbols;
  printf
    "\n%-26s %-10s %-10s %14s %14s\n"
    "scenario"
    "algo"
    "engine"
    "avg value add"
    "avg shortfall";
  List.sort rows ~compare:Poly.compare
  |> List.group ~break:(fun (s1, a1, e1, _, _) (s2, a2, e2, _, _) ->
    not (String.equal s1 s2 && String.equal a1 a2 && String.equal e1 e2))
  |> List.iter ~f:(fun group ->
    let scenario, algo, engine, (_ : int), (_ : int) = List.hd_exn group in
    let mean f =
      List.sum (module Int) group ~f // List.length group /. 100.
    in
    printf
      "%-26s %-10s %-10s %14.2f %14.2f\n"
      scenario
      algo
      engine
      (mean (fun (_, _, _, value_add, _) -> value_add))
      (mean (fun (_, _, _, _, shortfall) -> shortfall)));
  printf
    "\nruns checked: %d, violations: %d\n"
    (List.length rows)
    !violations;
  if !violations > 0 then exit 1
;;
