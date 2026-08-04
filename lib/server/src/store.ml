open! Core
open! Execlab_types
open Execlab_protocol

module Record = struct
  type t =
    { config : Run_config.t
    ; summary : Run_summary.t
    ; submitted_at : string
    }
  [@@deriving sexp]

  let row t =
    { Leaderboard_row.player = t.config.player
    ; algo_name = t.config.algo_name
    ; submitted_at = t.submitted_at
    ; summary = t.summary
    }
  ;;
end

(* One board per (symbol, day, alpha, engine): a bar-model run and a
   synthetic-exchange run of the same alpha are different contests. *)
let board_dir ~runs_dir ~symbol ~date ~alpha_hash ~engine_name ~physics =
  runs_dir
  ^/ sprintf
       "%s-%s-%s-%s-%s"
       (Symbol.to_string symbol)
       (Date.to_string date)
       alpha_hash
       (match engine_name with
        | "synthetic" -> "synthetic"
        | (_ : string) -> "bar")
       physics
;;

let save ~runs_dir ~physics (record : Record.t) =
  let dir =
    board_dir
      ~runs_dir
      ~symbol:record.config.symbol
      ~date:record.config.date
      ~alpha_hash:(alpha_hash record.config.alpha_text)
      ~engine_name:record.config.engine_name
      ~physics
  in
  Core_unix.mkdir_p dir;
  let file =
    dir
    ^/ sprintf
         "%s-%s.sexp"
         record.submitted_at
         (String.prefix
            (Md5.to_hex
               (Md5.digest_string
                  (Sexp.to_string [%sexp (record : Record.t)])))
            8)
  in
  Out_channel.write_all
    file
    ~data:(Sexp.to_string_hum [%sexp (record : Record.t)])
;;

let load_board ~runs_dir ~symbol ~date ~alpha_hash ~engine_name ~physics =
  let dir =
    board_dir ~runs_dir ~symbol ~date ~alpha_hash ~engine_name ~physics
  in
  match Sys_unix.readdir dir with
  | exception (_ : exn) -> []
  | files ->
    Array.to_list files
    |> List.filter ~f:(fun file -> String.is_suffix file ~suffix:".sexp")
    |> List.filter_map ~f:(fun file ->
      Option.try_with (fun () ->
        [%of_sexp: Record.t] (Sexp.load_sexp (dir ^/ file))))
    |> List.map ~f:Record.row
    |> List.sort
         ~compare:
           (Comparable.lift
              (Comparable.reverse Int63.compare)
              ~f:(fun (row : Leaderboard_row.t) ->
                row.summary.value_add_cents))
;;
