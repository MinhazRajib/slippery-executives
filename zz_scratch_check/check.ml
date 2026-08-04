open! Core
open Execlab_protocol

let body =
  {|((player traversal-poc)(symbol ../data/AAPL)(date 2026-07-09)(alpha_text "10:00:00,AAPL,BUY,5000,11:00:00\n")(algo_name twap)(half_spread_cents 2)(max_participation 0.1)(impact_coefficient_cents 25)(pov_rate 0.0015)(is_urgency 2)(engine_name bar)(engine_seed 0))|}
;;

let () =
  (match
     Or_error.try_with (fun () ->
       [%of_sexp: Run_config.t] (Sexp.of_string body))
   with
   | Ok (cfg : Run_config.t) ->
     print_s [%message "ACCEPTED" (cfg.symbol : Execlab_types.Symbol.t)]
   | Error e -> print_s [%message "REJECTED" (e : Error.t)]);
  match
    Or_error.try_with (fun () ->
      [%of_sexp: Run_config.t]
        (Sexp.of_string
           (String.substr_replace_first
              body
              ~pattern:"../data/AAPL"
              ~with_:"AAPL")))
  with
  | Ok (cfg : Run_config.t) ->
    print_s [%message "ACCEPTED" (cfg.symbol : Execlab_types.Symbol.t)]
  | Error e -> print_s [%message "REJECTED" (e : Error.t)]
;;
