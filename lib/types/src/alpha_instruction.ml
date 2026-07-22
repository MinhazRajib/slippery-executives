open! Core

type t =
  { arrival_time : Time_ns.Ofday.t
  ; symbol : Symbol.t
  ; side : Side.t
  ; quantity : int
  ; deadline : Time_ns.Ofday.t
  }
[@@deriving sexp, bin_io, compare, equal]

let create ~arrival_time ~symbol ~side ~quantity ~deadline =
  if quantity <= 0
  then
    Or_error.error_s [%message "Quantity must be positive" (quantity : int)]
  else if Time_ns.Ofday.compare arrival_time deadline > 0
  then
    Or_error.error_s
      [%message
        "Arrival time must be before or equal to deadline"
          (arrival_time : Time_ns.Ofday.t)
          (deadline : Time_ns.Ofday.t)]
  else if Time_ns.Ofday.compare
            arrival_time
            (Time_ns.Ofday.of_string "09:30:00")
          < 0
  then
    Or_error.error_s
      [%message
        "Arrival time must be after or equal to 09:30:00"
          (arrival_time : Time_ns.Ofday.t)]
  else if Time_ns.Ofday.compare deadline (Time_ns.Ofday.of_string "16:00:00")
          > 0
  then
    Or_error.error_s
      [%message
        "Deadline must be before or equal to 16:00:00"
          (deadline : Time_ns.Ofday.t)]
  else if Time_ns.Ofday.compare
            arrival_time
            (Time_ns.Ofday.of_string "16:00:00")
          > 0
  then
    Or_error.error_s
      [%message
        "Arrival time must be before or equal to 16:00:00"
          (arrival_time : Time_ns.Ofday.t)]
  else if Time_ns.Ofday.compare deadline (Time_ns.Ofday.of_string "09:30:00")
          < 0
  then
    Or_error.error_s
      [%message
        "Deadline must be after or equal to 09:30:00"
          (deadline : Time_ns.Ofday.t)]
  else Ok { arrival_time; symbol; side; quantity; deadline }
;;
