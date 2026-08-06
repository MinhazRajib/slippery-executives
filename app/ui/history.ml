(* The dashboard's run history: one record per completed simulation,
   persisted through {!Storage} so it survives reloads. (Record shape adapted
   from app/ui/client/model.ml's Run_record.) *)

open! Core
open! Execlab_types

module Run_record = struct
  type t =
    { symbols : Symbol.t list
        (* every name the run's alpha touched, ascending *)
    ; date : Date.t
    ; algo_name : string
    ; alpha_capture : float option
    ; value_add_cents : int
    ; net_cents : int
    ; shortfall_bps : float
        (* fill-weighted across the run's orders; positive is worse *)
    ; completion : float (* filled / ordered, 0 to 1 *)
    ; alpha_text : string
         [@sexp.default ""]
         (* the exact instructions, so the run can be replayed; "" on records
            saved before configs were stored — Open then errors visibly *)
    ; half_spread_cents : int [@sexp.default 2]
    ; max_participation : float [@sexp.default 0.1]
    ; impact_coefficient_cents : int [@sexp.default 25]
    ; pov_rate : float [@sexp.default 0.0015]
    ; is_urgency : float [@sexp.default 1.0]
    ; engine_name : string [@sexp.default "bar"] (* "bar" | "synthetic" *)
    ; engine_seed : int [@sexp.default 1]
    ; ran_at : string [@sexp.default ""]
    (* wall-clock label, newest-first tiebreak *)
    }
  [@@deriving sexp, equal] [@@sexp.allow_extra_fields]

  (* Identity for delete/compare: the config plus when it ran. *)
  let id t =
    Md5.to_hex (Md5.digest_string (Sexp.to_string [%sexp (t : t)]))
    |> fun hex -> String.prefix hex 12
  ;;

  (* History written before a run could name more than one symbol carries
     [(symbol TSLA)]. One unreadable record would take the whole list down
     with it — {!load} falls back to empty — so upgrade the field instead. *)
  let t_of_sexp sexp =
    let sexp =
      match sexp with
      | Sexp.Atom (_ : string) -> sexp
      | Sexp.List fields ->
        Sexp.List
          (List.map fields ~f:(function
            | Sexp.List [ Sexp.Atom "symbol"; symbol ] ->
              Sexp.List [ Sexp.Atom "symbols"; Sexp.List [ symbol ] ]
            | field -> field))
    in
    t_of_sexp sexp
  ;;

  (* "TSLA", or "AAPL +2" when the alpha spanned several names — a history
     row has one line to say what was traded. *)
  let symbols_label t =
    match t.symbols with
    | [] -> "-"
    | [ symbol ] -> Symbol.to_string symbol
    | first :: rest ->
      sprintf "%s +%d" (Symbol.to_string first) (List.length rest)
  ;;
end

let capacity = 20

let load () =
  match Storage.get Storage.runs_key with
  | None -> []
  | Some text ->
    (try [%of_sexp: Run_record.t list] (Sexp.of_string text) with
     | (_ : exn) -> [])
;;

let save runs =
  let runs = List.take runs capacity in
  Storage.set
    Storage.runs_key
    (Sexp.to_string [%sexp (runs : Run_record.t list)])
;;

let add run runs =
  let runs = List.take (run :: runs) capacity in
  save runs;
  runs
;;

let remove ~id runs =
  let runs =
    List.filter runs ~f:(fun run ->
      not (String.equal (Run_record.id run) id))
  in
  save runs;
  runs
;;

let clear () = save []
