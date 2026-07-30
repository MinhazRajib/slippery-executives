(* Style tokens: financial-paper theme. Warm cream page, ivory cards, oxford
   navy primary, ink text; green/red reserved for good/bad execution results. *)

open! Core
open! Bonsai_web

let page_bg = "#f6ede2"
let card_bg = "#fffaf3"
let border = "1px solid #e8dbc9"
let hairline = "#f0e6d8"
let chip_bg = "#efe4d3" (* button groups, progress tracks, inactive pills *)
let text = "#33302e"
let secondary = "#6b645d"
let faint = "#a49c92"
let blue = "#0f5499"
let blue_soft = "#e0eaf3"
let green = "#15803d"
let red = "#b91c1c"
let orange = "#d97706" (* the vwap reference line *)

(* Identity colors for orders: never good/bad green/red, never the orange
   vwap line, never the navy price line. *)
let order_colors = [| "#6d28d9"; "#0e7490"; "#be185d"; "#0f766e" |]
let order_color index = order_colors.(index % Array.length order_colors)

let mono =
  "font-family:ui-monospace,'SF \
   Mono',Menlo,monospace;font-variant-numeric:tabular-nums;"
;;

let shadow = "box-shadow:0 1px 2px rgba(67,53,34,0.05);"
let s = Vdom.Attr.create "style"

let card extra =
  s
    ("background:"
     ^ card_bg
     ^ ";border:"
     ^ border
     ^ ";border-radius:6px;"
     ^ shadow
     ^ extra)
;;

let label =
  "color:" ^ faint ^ ";font-size:12px;font-weight:600;letter-spacing:0.02em;"
;;

let brand =
  s ("color:" ^ secondary ^ ";font-size:12px;font-weight:600;" ^ mono)
;;

let code_chip =
  s
    ("background:"
     ^ chip_bg
     ^ ";border:1px solid #e2d4bf;border-radius:5px;padding:1px \
        6px;font-size:12px;color:"
     ^ secondary
     ^ ";"
     ^ mono)
;;
