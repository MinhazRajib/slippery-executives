open! Core

type t =
  | Day
  | IOC
[@@deriving
  sexp
  , bin_io
  , compare
  , equal
  , enumerate
  , hash
  , string ~case_insensitive ~capitalize:"SCREAMING_SNAKE_CASE"]

let rests_on_book t = match t with Day -> true | IOC -> false
let all_str = List.map all ~f:to_string |> String.concat ~sep:", "
