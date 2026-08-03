(* The dashboard's run history: one record per completed simulation,
   persisted through {!Storage} so it survives reloads. (Record shape adapted
   from app/ui/client/model.ml's Run_record.) *)

open! Core
open! Execlab_types

module Run_record = struct
  type t =
    { symbol : Symbol.t
    ; date : Date.t
    ; algo_name : string
    ; alpha_capture : float option
    ; value_add_cents : int
    ; net_cents : int
    }
  [@@deriving sexp, equal]
end

let capacity = 20

let load () =
  match Storage.get Storage.runs_key with
  | None -> []
  | Some text ->
    (try [%of_sexp: Run_record.t list] (Sexp.of_string text) with
     | (_ : exn) -> [])
;;

let save runs =
  let runs = List.take runs capacity in
  Storage.set
    Storage.runs_key
    (Sexp.to_string [%sexp (runs : Run_record.t list)])
;;

let add run runs =
  let runs = run :: runs in
  save runs;
  runs
;;
