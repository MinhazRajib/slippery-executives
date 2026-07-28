open! Core
include Int

let to_int = Fn.id
let of_int = Fn.id
let to_float t = Int.to_float (to_int t)
