open! Core
open! Execlab_types

module Run_config = struct
  type t =
    { player : string
    ; symbol : Symbol.t
    ; date : Date.t
    ; alpha_text : string
    ; algo_name : string
    ; half_spread_cents : int
    ; max_participation : float
    ; impact_coefficient_cents : int
    ; pov_rate : float
    ; is_urgency : float
    ; engine_name : string
    ; engine_seed : int
    }
  [@@deriving sexp, equal]
end

module Run_summary = struct
  type t =
    { value_add_cents : Int63.t
    ; net_cents : Int63.t
    ; gross_cents : Int63.t
    ; alpha_capture : float option
    ; shortfall_cents : Int63.t
    }
  [@@deriving sexp, equal]

  let of_cents ~value_add ~net ~gross ~alpha_capture ~shortfall =
    { value_add_cents = Int63.of_int value_add
    ; net_cents = Int63.of_int net
    ; gross_cents = Int63.of_int gross
    ; alpha_capture
    ; shortfall_cents = Int63.of_int shortfall
    }
  ;;

  let dollars cents = Int63.to_float cents /. 100.
end

module Leaderboard_row = struct
  type t =
    { player : string
    ; algo_name : string
    ; submitted_at : string
    ; summary : Run_summary.t
    }
  [@@deriving sexp, equal]
end

module Submit_run = struct
  let path = "/api/submit-run"

  module Request = Run_config

  module Response = struct
    type t =
      { summary : Run_summary.t
      ; leaderboard : Leaderboard_row.t list
      }
    [@@deriving sexp, equal]
  end
end

module Leaderboard = struct
  let path = "/api/leaderboard"

  module Request = struct
    type t =
      { symbol : Symbol.t
      ; date : Date.t
      ; alpha_hash : string
      ; engine_name : string
      }
    [@@deriving sexp, equal]
  end

  module Response = struct
    type t = { rows : Leaderboard_row.t list } [@@deriving sexp, equal]
  end
end

let alpha_hash text =
  let canonical =
    String.split_lines text
    |> List.map ~f:String.strip
    |> List.filter ~f:(fun line -> not (String.is_empty line))
    |> String.concat ~sep:"\n"
  in
  Md5.to_hex (Md5.digest_string canonical)
;;
