(* Tiny localStorage wrapper: the theme choice and the run history survive
   page reloads — the lab's only persistence. Values are plain strings;
   callers own the encoding (Sexp for structured data). Everything degrades
   silently when storage is unavailable (private browsing, disabled). *)

open! Core
open Js_of_ocaml

let get key =
  Js.Optdef.case
    Dom_html.window##.localStorage
    (fun () -> None)
    (fun storage ->
      Js.Opt.case
        (storage##getItem (Js.string key))
        (fun () -> None)
        (fun value -> Some (Js.to_string value)))
;;

let set key value =
  Js.Optdef.iter Dom_html.window##.localStorage (fun storage ->
    storage##setItem (Js.string key) (Js.string value))
;;

let theme_key = "execlab-theme"
let runs_key = "execlab-runs"
