(* Style tokens: dark terminal theme. Near-black page with a blue cast, dark
   navy cards, whitish ink; green/red reserved for good/bad execution
   results. *)

open! Core
open! Bonsai_web

let page_bg = "#0a0e1a"
let card_bg = "#111726"
let border = "1px solid #212b40"
let hairline = "#1a2234"
let chip_bg = "#1c2536" (* button groups, progress tracks, inactive pills *)
let text = "#e6eaf2"
let secondary = "#9aa5bb"
let faint = "#64708a"
let blue = "#3b82f6"
let blue_soft = "#16243d"
let green = "#22c55e"
let red = "#ef4444"
let orange = "#f59e0b" (* the vwap reference line *)

(* Identity colors for orders: never good/bad green/red, never the orange
   vwap line, never the blue price line. Lifted for contrast on dark. *)
let order_colors = [| "#a78bfa"; "#22d3ee"; "#f472b6"; "#2dd4bf" |]
let order_color index = order_colors.(index % Array.length order_colors)

let mono =
  "font-family:ui-monospace,'SF \
   Mono',Menlo,monospace;font-variant-numeric:tabular-nums;"
;;

let shadow = "box-shadow:0 1px 3px rgba(0,0,0,0.4);"
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
     ^ ";border:1px solid #26314a;border-radius:5px;padding:1px \
        6px;font-size:12px;color:"
     ^ secondary
     ^ ";"
     ^ mono)
;;
