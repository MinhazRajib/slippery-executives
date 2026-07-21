open! Core

type t =
  { fill_id : int
  ; symbol : Symbol.t
  ; price : Price.t
  ; size : Size.t
  ; order_id : Order_id.t
  ; side : Side.t
  ; time : Time_ns.Ofday.t
  ; liquidity : Liquidity.t
  }
[@@deriving sexp, bin_io]

let to_string t =
  [%string
    "%{t.time#Time_ns.Ofday} %{t.side#Side} %{t.size#Size} \
     %{t.symbol#Symbol} @ %{Price.to_string_dollar t.price} \
     (%{t.liquidity#Liquidity}, fill %{t.fill_id#Int}, order \
     %{t.order_id#Order_id})"]
;;

let notional_cents t = Price.to_int_cents t.price * Size.to_int t.size
