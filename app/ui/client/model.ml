(* The single client-side application state: which screen is showing and
   everything the wizard has collected so far. One record, updated
   functionally — screens are pure views over it, and untouched fields keep
   physical equality so Bonsai's destructuring cutoffs work. *)

open! Core
open! Execlab_types

module Theme = struct
  type t =
    | Dark
    | Light
  [@@deriving sexp, compare, equal]

  let flip = function Dark -> Light | Light -> Dark
end

module Screen = struct
  (* Wizard flow: Dashboard -> Choose_day -> Alpha -> Algo -> Confirm ->
     Simulate -> Results. *)
  type t =
    | Dashboard
    | Choose_day
    | Alpha
    | Algo
    | Confirm
    | Simulate
    | Results
  [@@deriving sexp_of, compare, equal]

  (* Position in the wizard step bar; the dashboard is outside it. *)
  let step_index = function
    | Dashboard -> None
    | Choose_day -> Some 0
    | Alpha -> Some 1
    | Algo -> Some 2
    | Confirm -> Some 3
    | Simulate -> Some 4
    | Results -> Some 5
  ;;
end

module Run_record = struct
  (* One row of the dashboard's Recent/Best runs lists. Serializable so the
     history can persist in localStorage across page reloads. *)
  type t =
    { symbol : Symbol.t
    ; date : Date.t
    ; algo_name : string
    ; alpha_capture : float option
    ; value_add_cents : int
    ; net_cents : int
    }
  [@@deriving sexp, equal]
end

type t =
  { screen : Screen.t
  ; theme : Theme.t
  ; selection : (Symbol.t * Date.t) option
  ; alpha_text : string
  ; algo : Sim.Algo_choice.t
  ; half_spread_text : string (** dollars, e.g. ["0.02"] *)
  ; participation_text : string (** fraction of bar volume, e.g. ["0.10"] *)
  ; impact_text : string (** dollars at 100% participation, e.g. ["0.10"] *)
  ; run_error : Error.t option
  (** last failed run attempt, shown on Confirm *)
  ; runs : Run_record.t list (** newest first *)
  ; output : Sim.Output.t option
  (** what Simulate replays and Results grades *)
  ; sim_minute : int (** playback position, 0..389 *)
  ; sim_playing : bool
  ; sim_speed : int (** bars advanced per second of wall clock *)
  }

(* Friction defaults mirror [Fill_model.Config.default]. *)
let initial =
  { screen = Screen.Dashboard
  ; theme = Theme.Dark
  ; selection = None
  ; alpha_text = ""
  ; algo = Sim.Algo_choice.Twap
  ; half_spread_text = "0.02"
  ; participation_text = "0.10"
  ; impact_text = "0.10"
  ; run_error = None
  ; runs = []
  ; output = None
  ; sim_minute = 0
  ; sim_playing = false
  ; sim_speed = 2
  }
;;

(* The updater every screen gets: apply a functional update to the model. *)
type updater = (t -> t) -> unit Bonsai_web.Effect.t
