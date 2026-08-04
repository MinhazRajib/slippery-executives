open! Core

module T = struct
  type t = string [@@deriving sexp, bin_io, compare, equal, hash, string]
end

include T
include Comparable.Make (T)
include Hashable.Make (T)

let of_string s =
  if String.is_empty s
  then raise_s [%message "Symbol.of_string: symbol must be non-empty"];
  if String.for_all s ~f:Char.is_uppercase
  then s
  else raise_s [%message "Symbol.of_string: symbol must be uppercase"]
;;

(* Sexp is a human/wire format (it arrives over the server protocol), so
   deserialization must revalidate — the derived reader would admit any
   string, including path fragments like "..". [bin_io] stays raw: machine
   formats need no validation. *)
let t_of_sexp sexp = of_string (String.t_of_sexp sexp)
