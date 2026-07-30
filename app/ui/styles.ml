(* Style tokens: dense violet terminal. Sharp corners, hairline borders,
   tabular numerals everywhere. *)

open! Core
open! Bonsai_web

let bg0 = "#0d0918"
let bg1 = "#141024"
let bg2 = "#1d1732"
let hairline = "rgba(255,255,255,0.08)"
let border = "1px solid rgba(255,255,255,0.08)"
let text = "#e9e4ff"
let dim = "#a89fd4"
let faint = "#675e92"
let accent = "#8b5cf6"
let accent_bright = "#a78bfa"
let accent_soft = "rgba(139,92,246,0.15)"
let green = "#2fd575"
let orange = "#f5a623"
let red = "#f0506e"

let mono =
  "font-family:ui-monospace,'SF \
   Mono',Menlo,monospace;font-variant-numeric:tabular-nums;"
;;

let s = Vdom.Attr.create "style"

let panel extra =
  s
    ("background:"
     ^ bg1
     ^ ";border:"
     ^ border
     ^ ";border-radius:2px;"
     ^ extra)
;;

(* Uppercase micro-label used for panel titles and table headers. *)
let micro =
  "color:"
  ^ faint
  ^ ";font-size:10px;letter-spacing:0.08em;text-transform:uppercase;font-weight:700;"
;;

let panel_title = s (micro ^ "padding:8px 10px;border-bottom:" ^ border ^ ";")
let cell = "font-size:12px;color:" ^ text ^ ";" ^ mono
let dim_cell = "font-size:12px;color:" ^ dim ^ ";" ^ mono

let dot color =
  s
    ("display:inline-block;width:6px;height:6px;border-radius:9999px;background:"
     ^ color
     ^ ";")
;;
