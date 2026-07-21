open! Core

type t =
  { timestamp : Time_ns.t
  ; symbol : Symbol.t
  ; side : Side.t
  ; quantity : int
  ; deadline : Time_ns.t
  }
