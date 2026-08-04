(** A tiny pure LCG over [Int32] — not statistical quality, but exactly
    reproducible {e across platforms}: js_of_ocaml's native [int] is 32
    bits, so ordinary [int] arithmetic diverges between the browser and
    the server, and the anti-cheat story (the server re-runs a client's
    config and expects identical fills) demands bit-equality.

    Every draw returns the advanced state alongside its value, so a
    caller threads the generator explicitly and two runs of the same
    seed replay identically. [float] is uniform in [0, 1); [jitter]
    scatters a value uniformly within a relative [spread] of [around];
    [bernoulli] is [true] with probability [p]. *)

open! Core

type t

val create : seed:int -> t
val float : t -> t * float
val jitter : t -> around:float -> spread:float -> t * float
val bernoulli : t -> p:float -> t * bool
