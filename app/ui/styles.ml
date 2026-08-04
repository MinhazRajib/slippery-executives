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

(* A cool "instrument panel" light theme: blue-grey slate page, pure white
   cards so panels read as lifted surfaces. Order colors are spaced >= 18
   CIEDE2000 from each other and from every reserved hue (blue price line,
   orange vwap, green/red good-bad), so no two order marks are confusable. *)
let paper =
  { page_bg = "#e8ecf3"
  ; card_bg = "#ffffff"
  ; border = "1px solid #d2dae7"
  ; hairline = "#e6ebf3"
  ; chip_bg = "#edf1f8"
  ; text = "#0f141c"
  ; secondary = "#4c5a6d"
  ; faint = "#5f6c80"
  ; blue = "#1857c9"
  ; blue_soft = "#cfe0ff"
  ; green = "#0b7a55"
  ; red = "#c81e3a"
  ; orange = "#c2700a"
  ; brown = "#6e3a24"
  ; order_colors = [| "#7a24bd"; "#0f7d95"; "#c02a86"; "#6b6a12" |]
  ; chip_border = "#dce3ee"
  ; shadow =
      "box-shadow:0 1px 2px rgba(20,32,54,0.05),0 2px 6px \
       rgba(20,32,54,0.04),0 8px 24px rgba(20,32,54,0.06);"
  }
;;

let dark =
  { page_bg = "#080b11"
  ; card_bg = "#121822"
  ; border = "1px solid #26303f"
  ; hairline = "#1c2431"
  ; chip_bg = "#1b2230"
  ; text = "#e9eef7"
  ; secondary = "#a0adc0"
  ; faint = "#8492a8"
  ; blue = "#2f7dff"
  ; blue_soft = "#132444"
  ; green = "#2fd08a"
  ; red = "#ff5c6c"
  ; orange = "#f2a63b"
  ; brown = "#cf8a6a"
  ; order_colors = [| "#b98cff"; "#22d3ee"; "#e14aad"; "#b9d94e" |]
  ; chip_border = "#2b3546"
  ; shadow =
      "box-shadow:0 1px 3px rgba(0,0,0,0.35),0 3px 8px rgba(0,0,0,0.25),0 \
       10px 28px rgba(0,0,0,0.2);"
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
     ^ ";border-radius:10px;"
     ^ t.shadow
     ^ extra)
;;

let label t =
  "color:"
  ^ t.faint
  ^ ";font-size:12px;font-weight:600;letter-spacing:0.02em;"
;;

(* Table column headers: the warm accent — rust on paper, tan on dark —
   instead of washed-out grey. *)
let table_label t =
  "color:"
  ^ t.brown
  ^ ";font-size:11px;font-weight:700;letter-spacing:0.06em;text-transform:uppercase;"
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
