(* Style tokens: light institutional dashboard. Grey page, white cards with
   soft shadows, blue primary, green/red reserved for good/bad execution
   results. *)

open! Core
open! Bonsai_web

let page_bg = "#f1f3f7"
let card_bg = "#ffffff"
let border = "1px solid #e6e9f0"
let hairline = "#eceff5"
let text = "#1b2436"
let secondary = "#5b6478"
let faint = "#9aa3b8"
let blue = "#2563eb"
let blue_soft = "#e8eefc"
let green = "#16a34a"
let red = "#dc2626"
let orange = "#f59e0b" (* the vwap reference line *)

(* Identity colors for orders: never good/bad green/red, never the orange
   vwap line, never the blue price line. *)
let order_colors = [| "#7c3aed"; "#0891b2"; "#db2777"; "#0d9488" |]
let order_color index = order_colors.(index % Array.length order_colors)

let mono =
  "font-family:ui-monospace,'SF \
   Mono',Menlo,monospace;font-variant-numeric:tabular-nums;"
;;

let shadow =
  "box-shadow:0 1px 2px rgba(16,24,40,0.05),0 1px 3px rgba(16,24,40,0.08);"
;;

let s = Vdom.Attr.create "style"

let card extra =
  s
    ("background:"
     ^ card_bg
     ^ ";border:"
     ^ border
     ^ ";border-radius:10px;"
     ^ shadow
     ^ extra)
;;

let label =
  "color:" ^ faint ^ ";font-size:12px;font-weight:600;letter-spacing:0.02em;"
;;

let eyebrow =
  s
    ("color:"
     ^ faint
     ^ ";font-size:11px;font-weight:700;letter-spacing:0.12em;text-transform:uppercase;"
    )
;;

let code_chip =
  s
    ("background:#eef1f6;border:1px solid \
      #e2e6ee;border-radius:5px;padding:1px 6px;font-size:12px;color:"
     ^ secondary
     ^ ";"
     ^ mono)
;;
