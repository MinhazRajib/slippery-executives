(* Client-side file download: encode the text as a [data:] URI and click a
   synthetic anchor with a [download] attribute — no server involved. *)

open! Core
open Js_of_ocaml

let csv ~filename ~text =
  let anchor = Dom_html.createA Dom_html.document in
  anchor##.href
  := Js.string ("data:text/csv;charset=utf-8," ^ Url.urlencode text);
  anchor##setAttribute (Js.string "download") (Js.string filename);
  anchor##click
;;
