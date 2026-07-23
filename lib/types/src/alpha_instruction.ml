open! Core

type t =
  { arrival_time : Time_ns.Ofday.t
  ; symbol : Symbol.t
  ; side : Side.t
  ; quantity : Size.t
  ; deadline : Time_ns.Ofday.t
  }
[@@deriving sexp, bin_io, compare, equal]

let market_open = Time_ns.Ofday.of_string "09:30:00"
let market_close = Time_ns.Ofday.of_string "16:00:00"

let create ~arrival_time ~symbol ~side ~quantity ~deadline =
  if Size.to_int quantity <= 0
  then
    Or_error.error_s
      [%message "Quantity must be positive" (quantity : Size.t)]
  else if Time_ns.Ofday.compare arrival_time deadline > 0
  then
    Or_error.error_s
      [%message
        "Arrival time must be before or equal to deadline"
          (arrival_time : Time_ns.Ofday.t)
          (deadline : Time_ns.Ofday.t)]
  else if Time_ns.Ofday.compare arrival_time market_open < 0
  then
    Or_error.error_s
      [%message
        "Arrival time must be after or equal to 09:30:00"
          (arrival_time : Time_ns.Ofday.t)]
  else if Time_ns.Ofday.compare deadline market_close > 0
  then
    Or_error.error_s
      [%message
        "Deadline must be before or equal to 16:00:00"
          (deadline : Time_ns.Ofday.t)]
  else Ok { arrival_time; symbol; side; quantity; deadline }
;;
