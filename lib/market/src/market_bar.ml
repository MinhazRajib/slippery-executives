open! Core
open! Execlab_types

type t =
  { time : Time_ns.Ofday.t
  ; open_ : Price.t
  ; high : Price.t
  ; low : Price.t
  ; close : Price.t
  ; volume : Size.t
  }
[@@deriving sexp, bin_io, compare, equal]

let create ~time ~open_ ~high ~low ~close ~volume =
  if Price.compare high low < 0
  then
    Or_error.error_s
      [%message
        "High price must be greater than or equal to low price"
          (high : Price.t)
          (low : Price.t)]
  else if Price.compare open_ low < 0 || Price.compare open_ high > 0
  then
    Or_error.error_s
      [%message
        "Open price must be between low and high prices"
          (open_ : Price.t)
          (low : Price.t)
          (high : Price.t)]
  else if Price.compare close low < 0 || Price.compare close high > 0
  then
    Or_error.error_s
      [%message
        "Close price must be between low and high prices"
          (close : Price.t)
          (low : Price.t)
          (high : Price.t)]
  else if Size.to_int volume < 0
  then
    Or_error.error_s
      [%message "Volume must be non-negative" (volume : Size.t)]
  else Ok { time; open_; high; low; close; volume }
;;
