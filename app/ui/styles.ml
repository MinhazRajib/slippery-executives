(* Style tokens for the execlab UI: deep violet-navy surfaces, one violet
   accent, green/orange/red status colors. Elevation tiers 0 (page) to 3
   (cards). *)

open! Core
open! Bonsai_web

let bg0 = "#120d20"
let bg1 = "#191231"
let bg2 = "#221a40"
let bg3 = "#2a2150"
let border = "1px solid rgba(255,255,255,0.07)"
let border_strong = "1px solid rgba(255,255,255,0.14)"
let text = "#ece8ff"
let text_dim = "#b3aad9"
let text_faint = "#7d74a8"
let accent = "#8b5cf6"
let accent_soft = "rgba(139,92,246,0.16)"
let green = "#22c55e"
let orange = "#f59e0b"
let red = "#ef4444"
let s attr = Vdom.Attr.create "style" attr

let panel extra =
  s
    ([%string
       "background:%{bg1};border:%{border};border-radius:10px;padding:14px;"]
     ^ extra)
;;

let mono =
  "font-family:ui-monospace,Menlo,monospace;font-variant-numeric:tabular-nums;"
;;

let label =
  s
    [%string
      "color:%{text_faint};font-size:11px;letter-spacing:0.08em;text-transform:uppercase;font-weight:600;"]
;;

let dot color =
  s
    [%string
      "display:inline-block;width:8px;height:8px;border-radius:9999px;background:%{color};"]
;;
