open! Core

type t = Int32.t

let create ~seed = Int32.of_int_trunc ((31 * seed) + 17)

(* Borland-style LCG on Int32: js_of_ocaml ints are 32-bit, so 63-bit int
   arithmetic would wrap differently in the browser than natively — and the
   server re-runs client configs expecting bit-identical fills. Int32 is
   exact on both. *)
let next state = Int32.((state * 1103515245l) + 12345l)

(* 15 high-ish bits, uniform in [0, 1). *)
let float state =
  let state = next state in
  let bits =
    Int32.to_int_trunc
      (Int32.bit_and (Int32.shift_right_logical state 16) 0x7FFFl)
  in
  state, Float.of_int bits /. 32768.
;;

(* Uniform in [around * (1 - spread), around * (1 + spread)). *)
let jitter state ~around ~spread =
  let state, unit = float state in
  state, around *. (1. +. (spread *. ((2. *. unit) -. 1.)))
;;

(* [true] with probability [p]. *)
let bernoulli state ~p =
  let state, unit = float state in
  state, Float.( < ) unit p
;;
