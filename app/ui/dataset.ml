(* The bundled market-data catalog. [Embedded_data] bakes every (symbol,
   date) CSV under [data/] into the client at build time (the browser has no
   filesystem); this module discovers what's available — driving the
   choose-a-day screen — and parses a chosen day into a validated
   {!Trading_day.t} via {!Data_loader.parse}. *)

open! Core
open! Execlab_types
open! Execlab_market

(* Symbols in alphabetical order, each with its dates ascending. *)
let catalog : (Symbol.t * Date.t list) list =
  Embedded_data.files
  |> List.map ~f:(fun (symbol, date, (_ : string)) ->
    Symbol.of_string symbol, Date.of_string date)
  |> Symbol.Map.of_alist_multi
  |> Map.map ~f:(List.sort ~compare:Date.compare)
  |> Map.to_alist
;;

let symbols = List.map catalog ~f:fst

let dates_for symbol =
  match List.Assoc.find catalog symbol ~equal:[%equal: Symbol.t] with
  | Some dates -> dates
  | None -> []
;;

module Day_summary = struct
  (* One row of the choose-day screen's session table. Return is
     close-vs-open in bps; prices are floats because they are statistics
     about the session, not tradeable prices. *)
  type t =
    { date : Date.t
    ; open_ : float
    ; close : float
    ; return_bps : float
    ; volume : int
    }
end

let load ~symbol ~date : Trading_day.t Or_error.t =
  match
    List.find Embedded_data.files ~f:(fun (s, d, (_ : string)) ->
      Symbol.equal symbol (Symbol.of_string s)
      && Date.equal date (Date.of_string d))
  with
  | None ->
    Or_error.error_s
      [%message "no bundled data" (symbol : Symbol.t) (date : Date.t)]
  | Some ((_ : string), (_ : string), contents) ->
    Data_loader.parse ~symbol ~date contents
;;

(* Parsed days are memoized: the choose-day preview re-renders on every state
   change and must not re-parse 390 rows each time. *)
let day_cache : Trading_day.t Or_error.t Hashtbl.M(String).t =
  Hashtbl.create (module String)
;;

let load_cached ~symbol ~date =
  Hashtbl.find_or_add
    day_cache
    (Symbol.to_string symbol ^ "/" ^ Date.to_string date)
    ~default:(fun () -> load ~symbol ~date)
;;

(* Parsing every session of a symbol costs ~4k rows, so summaries are
   memoized: the first visit to a symbol's calendar pays once. *)
let summary_cache : Day_summary.t list Hashtbl.M(Symbol).t =
  Hashtbl.create (module Symbol)
;;

let summaries_for symbol =
  Hashtbl.find_or_add summary_cache symbol ~default:(fun () ->
    List.filter_map (dates_for symbol) ~f:(fun date ->
      match load ~symbol ~date with
      | Error (_ : Error.t) -> None
      | Ok day ->
        let bars = day.Trading_day.bars in
        let open_ = Price.to_float (List.hd_exn bars).Market_bar.open_ in
        let close = Price.to_float (List.last_exn bars).Market_bar.close in
        let volume =
          List.sum (module Int) bars ~f:(fun bar ->
            Size.to_int bar.Market_bar.volume)
        in
        Some
          { Day_summary.date
          ; open_
          ; close
          ; return_bps = (close -. open_) /. open_ *. 10000.
          ; volume
          }))
;;
