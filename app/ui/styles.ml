(* Style tokens as a first-class theme so the app can switch palettes at
   runtime. [paper] is the cream editorial light theme — ink-navy text on
   warm paper, one royal blue, an amber kicker; [dark] is the blue-black
   terminal theme. Both keep green/red strictly for good/bad execution
   results. The look is a printed research note: serif display type, mono
   small-caps labels, hairline rules instead of heavy chrome. *)

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
  ; brown : string (* warm accent: kickers, replay controls *)
  ; order_colors : string array
      (* identity colors for orders: never good/bad green/red, never the
         orange vwap line, never the blue price line *)
  ; chip_border : string
  ; shadow : string
  }

(* The mockup's light theme: warm cream paper, ink-navy text, near-white
   panels that read as paper on paper. Order colors are spaced >= 18
   CIEDE2000 from each other and from every reserved hue (blue price line,
   orange vwap, green/red good-bad), so no two order marks are confusable. *)
let paper =
  { page_bg = "#f2efe8"
  ; card_bg = "#fbfaf6"
  ; border = "1px solid #d9d2c2"
  ; hairline = "#e4ded0"
  ; chip_bg = "#eae5d9"
  ; text = "#141f3c"
  ; secondary = "#46506a"
  ; faint = "#6d7488"
  ; blue = "#2440d8"
  ; blue_soft = "#dfe4f8"
  ; green = "#0d7a45"
  ; red = "#bf2d3d"
  ; orange = "#b96d0d"
  ; brown = "#b05a10"
  ; order_colors = [| "#7a24bd"; "#0f7d95"; "#c02a86"; "#6b6a12" |]
  ; chip_border = "#d3ccbb"
  ; shadow =
      "box-shadow:0 1px 2px rgba(42,34,18,0.05),0 6px 18px \
       rgba(42,34,18,0.04);"
  }
;;

let dark =
  { page_bg = "#0b0f17"
  ; card_bg = "#111724"
  ; border = "1px solid #242d3e"
  ; hairline = "#1c2434"
  ; chip_bg = "#1a2231"
  ; text = "#e8edf8"
  ; secondary = "#9aa7bf"
  ; faint = "#7e8aa2"
  ; blue = "#4c74f5"
  ; blue_soft = "#16264b"
  ; green = "#2fce87"
  ; red = "#ff5a68"
  ; orange = "#f0a43c"
  ; brown = "#e0913f"
  ; order_colors = [| "#b98cff"; "#22d3ee"; "#e14aad"; "#b9d94e" |]
  ; chip_border = "#2a3447"
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

(* Display serif for headlines only — body copy stays the system sans. *)
let serif = "font-family:'Iowan Old Style',Georgia,'Times New Roman',serif;"
let s = Vdom.Attr.create "style"

let card t extra =
  s
    ("background:"
     ^ t.card_bg
     ^ ";border:"
     ^ t.border
     ^ ";border-radius:4px;"
     ^ t.shadow
     ^ extra)
;;

(* The small-caps mono label that names every section and table column. *)
let label t =
  "color:"
  ^ t.faint
  ^ ";font-size:10.5px;font-weight:600;letter-spacing:0.12em;text-transform:uppercase;"
  ^ mono
;;

(* Table column headers share the label voice. *)
let table_label t = label t

(* The amber kicker line above page titles: the mockup's "HISTORICAL
   EXECUTION LABORATORY". *)
let kicker t =
  "color:"
  ^ t.brown
  ^ ";font-size:11px;font-weight:700;letter-spacing:0.16em;text-transform:uppercase;"
  ^ mono
;;

let code_chip t =
  s
    ("background:"
     ^ t.chip_bg
     ^ ";border:1px solid "
     ^ t.chip_border
     ^ ";border-radius:3px;padding:1px 6px;font-size:12px;color:"
     ^ t.secondary
     ^ ";"
     ^ mono)
;;
