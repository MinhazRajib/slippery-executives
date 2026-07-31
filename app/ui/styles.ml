(* Style tokens as a first-class theme so the app can switch palettes at
   runtime. [paper] is the warm light theme, [dark] the blue-black terminal
   theme; both keep green/red strictly for good/bad execution results. *)

open! Core
open! Bonsai_web

type t =
  { page_bg : string
  ; card_bg : string
  ; border : string
  ; hairline : string
  ; chip_bg : string (* button groups, progress tracks, inactive pills *)
  ; text : string
  ; secondary : string
  ; faint : string
  ; blue : string
  ; blue_soft : string
  ; green : string
  ; red : string
  ; orange : string (* the vwap reference line *)
  ; brown : string (* warm accent: replay controls, table accents *)
  ; order_colors : string array
      (* identity colors for orders: never good/bad green/red, never the
         orange vwap line, never the blue price line *)
  ; chip_border : string
  ; shadow : string
  }

let paper =
  { page_bg = "#f6ede2"
  ; card_bg = "#fffaf3"
  ; border = "1px solid #e8dbc9"
  ; hairline = "#f0e6d8"
  ; chip_bg = "#efe4d3"
  ; text = "#33302e"
  ; secondary = "#6b645d"
  ; faint = "#a49c92"
  ; blue = "#0f5499"
  ; blue_soft = "#e0eaf3"
  ; green = "#15803d"
  ; red = "#b91c1c"
  ; orange = "#d97706"
  ; brown = "#8b5e3c"
  ; order_colors = [| "#6d28d9"; "#0e7490"; "#be185d"; "#0f766e" |]
  ; chip_border = "#e2d4bf"
  ; shadow = "box-shadow:0 1px 2px rgba(67,53,34,0.05);"
  }
;;

let dark =
  { page_bg = "#0a0e1a"
  ; card_bg = "#111726"
  ; border = "1px solid #212b40"
  ; hairline = "#1a2234"
  ; chip_bg = "#1c2536"
  ; text = "#e6eaf2"
  ; secondary = "#9aa5bb"
  ; faint = "#64708a"
  ; blue = "#3b82f6"
  ; blue_soft = "#16243d"
  ; green = "#22c55e"
  ; red = "#ef4444"
  ; orange = "#f59e0b"
  ; brown = "#b98a5c"
  ; order_colors = [| "#a78bfa"; "#22d3ee"; "#f472b6"; "#2dd4bf" |]
  ; chip_border = "#26314a"
  ; shadow = "box-shadow:0 1px 3px rgba(0,0,0,0.4);"
  }
;;

let order_color t index =
  t.order_colors.(index % Array.length t.order_colors)
;;

let mono =
  "font-family:ui-monospace,'SF \
   Mono',Menlo,monospace;font-variant-numeric:tabular-nums;"
;;

let s = Vdom.Attr.create "style"

let card t extra =
  s
    ("background:"
     ^ t.card_bg
     ^ ";border:"
     ^ t.border
     ^ ";border-radius:6px;"
     ^ t.shadow
     ^ extra)
;;

let label t =
  "color:"
  ^ t.faint
  ^ ";font-size:12px;font-weight:600;letter-spacing:0.02em;"
;;

let brand t =
  s ("color:" ^ t.secondary ^ ";font-size:12px;font-weight:600;" ^ mono)
;;

let code_chip t =
  s
    ("background:"
     ^ t.chip_bg
     ^ ";border:1px solid "
     ^ t.chip_border
     ^ ";border-radius:5px;padding:1px 6px;font-size:12px;color:"
     ^ t.secondary
     ^ ";"
     ^ mono)
;;
