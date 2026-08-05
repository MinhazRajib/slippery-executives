open! Core
open! Execlab_types

type t =
  { date : Date.t
  ; days : Trading_day.t Symbol.Map.t
  ; bars : Market_bar.t array Symbol.Map.t
  ; times : Time_ns.Ofday.t array
  }

let of_days days =
  match days with
  | [] -> Or_error.error_string "Universe.of_days: no sessions"
  | (first : Trading_day.t) :: rest ->
    let open Or_error.Let_syntax in
    let%bind () =
      match
        List.find rest ~f:(fun day -> not (Date.equal day.date first.date))
      with
      | None -> Ok ()
      | Some day ->
        Or_error.error_s
          [%message
            "Universe.of_days: sessions are not the same date"
              ~first:(first.date : Date.t)
              ~also:(day.date : Date.t)]
    in
    let%map days =
      match
        Symbol.Map.of_alist
          (List.map (first :: rest) ~f:(fun day -> day.symbol, day))
      with
      | `Ok days -> Ok days
      | `Duplicate_key symbol ->
        Or_error.error_s
          [%message
            "Universe.of_days: two sessions for one symbol"
              (symbol : Symbol.t)]
    in
    { date = first.date
    ; days
    ; bars = Map.map days ~f:(fun day -> Array.of_list day.Trading_day.bars)
    ; times =
        Array.of_list
          (List.map first.bars ~f:(fun bar -> bar.Market_bar.time))
    }
;;

let of_day day = Or_error.ok_exn (of_days [ day ])
let date t = t.date
let symbols t = Map.keys t.days
let minutes t = Array.length t.times
let time_at t ~minute = t.times.(minute)
let day t symbol = Map.find t.days symbol

let day_exn t symbol =
  match day t symbol with
  | Some day -> day
  | None ->
    raise_s
      [%message
        "Universe: symbol is not in this run"
          (symbol : Symbol.t)
          ~universe:(Map.keys t.days : Symbol.t list)]
;;

let days t = Map.data t.days

let bar_exn t ~symbol ~minute =
  match Map.find t.bars symbol with
  | Some bars -> bars.(minute)
  | None ->
    raise_s
      [%message
        "Universe: symbol is not in this run"
          (symbol : Symbol.t)
          ~universe:(Map.keys t.days : Symbol.t list)]
;;

let mem t symbol = Map.mem t.days symbol
