(** The demo library: seven scenarios, each a real session whose shape makes
    one execution choice genuinely better than another, and each comparing
    configurations that differ in exactly one thing.

    [dune exec bin/demo.exe] runs them all; [dune exec bin/demo.exe -- 4]
    runs one. Every run is deterministic — fixed historical bars, a fixed
    seed for the synthetic engine, no wall-clock input — so the numbers
    printed here are the numbers anyone else gets.

    The prose in [demos/README.md] states what each case is meant to show;
    this executable is what proves it, and the outcomes are measured, not
    asserted. *)

open! Core
open Execlab_types
open! Execlab_market
open! Execlab_analytics
open! Execlab_simulation
open Execlab_session
open Execlab_server

let data_dir = "data"

module Config = struct
  (* One column of a demo: a label, the algorithm, and the parameters that
     differ from the defaults. *)
  type t =
    { label : string
    ; algo : string
    ; params : Params.t
    ; alpha : string option (* overrides the demo's, to vary the order *)
    }

  let make ?pov_rate ?urgency ?engine ?participation ?alpha label algo =
    let default = Params.default in
    { label
    ; algo
    ; params =
        { fill_config =
            (match participation with
             | None -> default.fill_config
             | Some max_participation ->
               { default.fill_config with max_participation })
        ; pov_rate = Option.value pov_rate ~default:default.pov_rate
        ; is_urgency = Option.value urgency ~default:default.is_urgency
        ; engine = Option.value engine ~default:default.engine
        }
    ; alpha
    }
  ;;
end

module Demo = struct
  type t =
    { index : int
    ; title : string
    ; symbol : string
    ; date : string
    ; alpha : string
    ; shows : string
    ; configs : Config.t list
    }
end

let demos =
  [ { Demo.index = 1
    ; title = "VWAP tracks a U-shaped day; TWAP trades away from the volume"
    ; symbol = "META"
    ; date = "2026-07-10"
    ; alpha = "demos/01_ushaped_vwap.csv"
    ; shows =
        "40% of this session's volume trades in the first and last half \
         hour. VWAP's mandate is to *track* the day's VWAP, not to beat it, \
         so read the vs-vwap column as distance from zero: shaping the \
         schedule to the volume curve lands nearer the benchmark than \
         spreading evenly does. Either may beat it on a given day by luck \
         of drift; tracking is the promise."
    ; configs =
        [ Config.make "vwap" "vwap"
        ; Config.make "twap" "twap"
        ; Config.make "immediate" "immediate"
        ]
    }
  ; { index = 2
    ; title = "A flat day punishes VWAP's forecast; TWAP's ignorance wins"
    ; symbol = "META"
    ; date = "2026-07-17"
    ; alpha = "demos/02_flat_twap.csv"
    ; shows =
        "The flattest volume profile in the catalog. VWAP still trades the \
         average META shape — heavy at the edges — because its forecast is \
         built from the *other* sessions. TWAP, which assumes nothing, is \
         closer to right."
    ; configs = [ Config.make "twap" "twap"; Config.make "vwap" "vwap" ]
    }
  ; { index = 3
    ; title = "POV keeps the smallest footprint — and waits for its volume"
    ; symbol = "META"
    ; date = "2026-07-17"
    ; alpha = "demos/03_spike_pov.csv"
    ; shows =
        "A single minute at 12:18 trades sixteen times the median, and no \
         forecast contains it. POV, defined by realized volume, does its \
         heaviest trading exactly there and books the lowest impact of the \
         three. It pays for that patience: the liquidity arrives late in a \
         rising tape, so its timing cost — and its shortfall — are the \
         worst. Cheapest footprint, worst drift; that is the trade, and the \
         decomposition separates the two."
    ; configs =
        [ Config.make ~pov_rate:0.03 "pov 3%" "pov"
        ; Config.make "twap" "twap"
        ; Config.make "vwap" "vwap"
        ]
    }
  ; { index = 4
    ; title = "Implementation shortfall outruns adverse drift"
    ; symbol = "NFLX"
    ; date = "2026-07-17"
    ; alpha = "demos/04_adverse_drift_is.csv"
    ; shows =
        "The tape runs 256bps against the buyer between 10:00 and 11:00. \
         Every minute of patience is paid for in timing cost, so the \
         front-loaded schedule keeps more of the arrival price — and the \
         cost decomposition says so in the timing column."
    ; configs =
        [ Config.make ~urgency:4. "is urgency 4" "is"
        ; Config.make ~urgency:2. "is urgency 2" "is"
        ; Config.make "twap" "twap"
        ; Config.make "immediate" "immediate"
        ]
    }
  ; { index = 5
    ; title = "When the signal decays in minutes, only immediacy captures it"
    ; symbol = "META"
    ; date = "2026-07-09"
    ; alpha = "demos/05_fast_decay_immediate.csv"
    ; shows =
        "125 of this hour's 153bps of adverse move land in the first five \
         minutes. The order is 9% of the arrival minute — inside the \
         participation cap, so immediacy can genuinely finish — and it \
         keeps almost all of the arrival price while every schedule buys \
         into the move. Sizing matters: make the same order five times \
         larger and immediacy cannot complete, which is demo 6."
    ; configs =
        [ Config.make "immediate" "immediate"
        ; Config.make ~urgency:4. "is urgency 4" "is"
        ; Config.make "twap" "twap"
        ]
    }
  ; { index = 6
    ; title = "The same order, three deadlines: what haste actually costs"
    ; symbol = "GOOG"
    ; date = "2026-07-13"
    ; alpha = "demos/06_impact_of_haste.csv"
    ; shows =
        "The quietest session in the catalog and one 120,000-share order, \
         executed by one algorithm, differing only in when it is due. \
         Compressing the deadline concentrates the order into fewer \
         minutes, and the impact bill and the unfilled remainder both grow \
         — a cost paid for urgency nobody asked for, since the alpha is the \
         same in all three."
    ; configs =
        [ Config.make
            ~alpha:"demos/06_impact_of_haste.csv"
            "due 15:59"
            "twap"
        ; Config.make
            ~alpha:"demos/06_impact_of_haste_hour.csv"
            "due 12:00"
            "twap"
        ; Config.make
            ~alpha:"demos/06_impact_of_haste_rushed.csv"
            "due 10:30"
            "twap"
        ]
    }
  ; { index = 7
    ; title = "One knob, both horns: participation rate"
    ; symbol = "GOOG"
    ; date = "2026-07-13"
    ; alpha = "demos/07_participation_tradeoff.csv"
    ; shows =
        "The same POV algorithm at three rates against 11% of the window's \
         volume. Low rates are cheap per share and may not finish; high \
         rates finish and pay for it. Completion rate and impact move in \
         opposite directions — the trade-off itself, with nothing else \
         changed."
    ; configs =
        [ Config.make ~pov_rate:0.02 "pov 2%" "pov"
        ; Config.make ~pov_rate:0.06 "pov 6%" "pov"
        ; Config.make ~pov_rate:0.15 "pov 15%" "pov"
        ]
    }
  ]
;;

let dollars cents = Float.of_int cents /. 100.

let run_demo (demo : Demo.t) =
  let symbol = Symbol.of_string demo.symbol in
  let date = Date.of_string demo.date in
  let day = Or_error.ok_exn (Catalog.load ~data_dir ~symbol ~date) in
  let forecast_days =
    Catalog.forecast_days ~data_dir ~symbol ~excluding:date
  in
  let instructions_of alpha =
    (Or_error.ok_exn
       (Execlab_alpha.Parser.parse (In_channel.read_all alpha)))
      .instructions
  in
  let requested_of instructions =
    List.sum (module Int) instructions ~f:(fun instruction ->
      Size.to_int instruction.Alpha_instruction.quantity)
  in
  let requested = requested_of (instructions_of demo.alpha) in
  printf "\n=== demo %d: %s\n" demo.index demo.title;
  printf
    "    %s %s · %s · %s shares\n"
    demo.symbol
    demo.date
    demo.alpha
    (Int.to_string_hum ~delimiter:',' requested);
  printf "    %s\n\n" demo.shows;
  printf
    "    %-14s %10s %10s %10s %10s %10s %10s\n"
    "config"
    "filled"
    "shortfall"
    "vs vwap"
    "timing $"
    "impact c/sh"
    "vs immed $";
  List.iter demo.configs ~f:(fun (config : Config.t) ->
    let instructions =
      instructions_of (Option.value config.alpha ~default:demo.alpha)
    in
    let requested = requested_of instructions in
    match
      Execlab_session.run
        ~universe:(Universe.of_day day)
        ~forecast_days:
          (Symbol.Map.singleton day.Trading_day.symbol forecast_days)
        ~instructions
        ~algo_name:config.algo
        ~params:config.params
    with
    | Error error ->
      printf !"    %-14s FAILED %{Error#hum}\n" config.label error
    | Ok outcome ->
      let filled =
        List.sum (module Int) outcome.graded ~f:(fun graded ->
          Size.to_int graded.Graded.grading.filled)
      in
      let mean f =
        List.sum (module Float) outcome.graded ~f
        /. Float.of_int (List.length outcome.graded)
      in
      printf
        "    %-14s %9.1f%% %9.1f %10.1f %10.2f %10.2f %10.2f\n"
        config.label
        (Float.of_int filled /. Float.of_int requested *. 100.)
        (mean (fun graded ->
           match graded.Graded.grading.fill_metrics with
           | None -> 0.
           | Some metrics -> metrics.shortfall_bps))
        (mean (fun graded ->
           match graded.Graded.grading.fill_metrics with
           | None -> 0.
           | Some metrics -> metrics.vwap_slippage_bps))
        (dollars
           (List.sum (module Int) outcome.graded ~f:(fun graded ->
              graded.Graded.grading.timing_cost_cents)))
        (* Per share, because a config that fills less also pays less in
           total: the question is what each share cost. *)
        (if filled = 0
         then 0.
         else
           Float.of_int
             (List.sum (module Int) outcome.graded ~f:(fun graded ->
                graded.Graded.grading.impact_cost_cents))
           /. Float.of_int filled)
        (dollars (Outcome.value_add_cents outcome)))
;;

let () =
  let chosen =
    match Sys.get_argv () with
    | [| _ |] -> demos
    | [| _; index |] ->
      let index = Int.of_string index in
      List.filter demos ~f:(fun demo -> demo.Demo.index = index)
    | _ ->
      eprintf "usage: demo.exe [1-7]\n";
      exit 2
  in
  printf
    "shortfall and vs-vwap are basis points (positive is worse); timing, \n\
     impact and vs-immediate are dollars.\n";
  List.iter chosen ~f:run_demo;
  printf "\n"
;;
