open! Core
open! Execlab_types

module Run_config = struct
  type t =
    { player : string
    ; symbols : Symbol.t list
    ; date : Date.t
    ; alpha_text : string
    ; algo_name : string
    ; half_spread_cents : int
    ; max_participation : float
    ; impact_coefficient_cents : int
    ; pov_rate : float
    ; is_urgency : float
    ; patience : float
    ; engine_name : string
    ; engine_seed : int
    }
  [@@deriving sexp, equal]

  (* Fields that arrived after runs were already on disk, with the value to
     read when a stored config predates them. A run graded before a knob
     existed cannot have used it — every one of these belongs to an algorithm
     that did not exist yet — so any value reproduces it; these are the
     defaults, kept here rather than reaching into {!Execlab_session}, which
     is downstream of this module. *)
  let defaults_for_older_configs = [ "patience", Sexp.Atom "0.5" ]

  (* A stored notebook and its leaderboard rows outlive the shape of the
     config that wrote them, and {!Store} reads them with [Option.try_with]:
     a record this reader rejects is not an error the user sees, it is a run
     that quietly disappears. So renamed fields are rewritten and missing
     ones are filled, rather than either being left to fail. *)
  let t_of_sexp sexp =
    let sexp =
      match sexp with
      | Sexp.Atom (_ : string) -> sexp
      | Sexp.List fields ->
        (* [symbol] became [symbols] when one alpha could name several. *)
        let fields =
          List.map fields ~f:(function
            | Sexp.List [ Sexp.Atom "symbol"; symbol ] ->
              Sexp.List [ Sexp.Atom "symbols"; Sexp.List [ symbol ] ]
            | field -> field)
        in
        let present name =
          List.exists fields ~f:(function
            | Sexp.List (Sexp.Atom key :: (_ : Sexp.t list)) ->
              String.equal key name
            | Sexp.Atom (_ : string) | List (_ : Sexp.t list) -> false)
        in
        Sexp.List
          (fields
           @ List.filter_map
               defaults_for_older_configs
               ~f:(fun (name, default) ->
                 if present name
                 then None
                 else Some (Sexp.List [ Sexp.Atom name; default ])))
    in
    t_of_sexp sexp
  ;;
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

module Credentials = struct
  type t =
    { username : string
    ; passcode : string
    }
  [@@deriving sexp, equal]
end

module Session = struct
  type t =
    { username : string
    ; token : string
    }
  [@@deriving sexp, equal]
end

module Create_account = struct
  let path = "/api/create-account"

  module Request = Credentials
  module Response = Session
end

module Sign_in = struct
  let path = "/api/sign-in"

  module Request = Credentials
  module Response = Session
end

module Leaderboard_row = struct
  type t =
    { player : string
    ; submitted_at : string
    ; summary : Run_summary.t
    }
  [@@deriving sexp, equal]
end

module Saved_run = struct
  type t =
    { run_id : string
    ; config : Run_config.t
    ; summary : Run_summary.t
    ; ran_at : string
    ; published : bool
    }
  [@@deriving sexp, equal]
end

module Save_run = struct
  let path = "/api/save-run"

  module Request = struct
    type t =
      { token : string
      ; config : Run_config.t
      }
    [@@deriving sexp, equal]
  end

  module Response = struct
    type t = { run : Saved_run.t } [@@deriving sexp, equal]
  end
end

module My_runs = struct
  let path = "/api/my-runs"

  module Request = struct
    type t = { token : string } [@@deriving sexp, equal]
  end

  module Response = struct
    type t = { runs : Saved_run.t list } [@@deriving sexp, equal]
  end
end

module Submit_run = struct
  let path = "/api/submit-run"

  module Request = struct
    type t =
      { token : string
      ; config : Run_config.t
      }
    [@@deriving sexp, equal]
  end

  module Response = struct
    type t =
      { summary : Run_summary.t
      ; leaderboard : Leaderboard_row.t list
      }
    [@@deriving sexp, equal]
  end
end

module Reset_account = struct
  let path = "/api/reset-account"

  module Request = struct
    type t = { token : string } [@@deriving sexp, equal]
  end

  module Response = struct
    type t = { deleted_runs : int } [@@deriving sexp, equal]
  end
end

module Leaderboard = struct
  let path = "/api/leaderboard"

  module Request = struct
    type t =
      { symbols : Symbol.t list
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
