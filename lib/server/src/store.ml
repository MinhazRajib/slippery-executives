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
    ; submitted_at = t.submitted_at
    ; summary = t.summary
    }
  ;;
end

let accounts_dir ~runs_dir = runs_dir ^/ "accounts"
let boards_dir ~runs_dir = runs_dir ^/ "boards"
let users_dir ~runs_dir = runs_dir ^/ "users"

let run_id ~(config : Run_config.t) ~ran_at =
  String.prefix
    (Md5.to_hex
       (Md5.digest_string
          (Sexp.to_string [%sexp (config : Run_config.t), (ran_at : string)])))
    16
;;

(* Path component for the run's names. [Symbol.of_string] admits uppercase
   letters only, so this is always filename-safe; long baskets collapse to a
   count, since [alpha_hash] already pins the exact file. *)
let symbols_slug symbols =
  match symbols with
  | [] -> "none"
  | symbols when List.length symbols <= 4 ->
    String.concat ~sep:"-" (List.map symbols ~f:Symbol.to_string)
  | first :: rest ->
    sprintf "%s-and%d" (Symbol.to_string first) (List.length rest)
;;

(* One board per (names, day, alpha, engine): a bar-model run and a
   synthetic-exchange run of the same alpha are different contests. *)
let board_dir ~runs_dir ~symbols ~date ~alpha_hash ~engine_name ~physics =
  boards_dir ~runs_dir
  ^/ sprintf
       "%s-%s-%s-%s-%s"
       (symbols_slug symbols)
       (Date.to_string date)
       alpha_hash
       (match engine_name with
        | "synthetic" -> "synthetic"
        | (_ : string) -> "bar")
       physics
;;

(* The only two places a caller-supplied string becomes a path component.
   Both are already constrained upstream — usernames by {!Accounts} and run
   ids by [run_id]'s hex — so this is an assertion of that, sited where the
   filesystem would pay for it being false. *)
let user_dir ~runs_dir ~username =
  if not (Accounts.is_filename_safe username)
  then
    raise_s
      [%message "username is not usable as a directory" (username : string)];
  users_dir ~runs_dir ^/ username
;;

let user_run_file ~runs_dir ~username ~run_id =
  if not (Accounts.is_filename_safe run_id)
  then
    raise_s [%message "run id is not usable as a filename" (run_id : string)];
  user_dir ~runs_dir ~username ^/ run_id ^ ".sexp"
;;

let load_sexp_file file ~of_sexp =
  Option.try_with (fun () -> of_sexp (Sexp.load_sexp file))
;;

let write_sexp_file file ~sexp =
  Out_channel.write_all file ~data:(Sexp.to_string_hum sexp)
;;

let save ~runs_dir ~physics (record : Record.t) =
  let dir =
    board_dir
      ~runs_dir
      ~symbols:record.config.symbols
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
  write_sexp_file file ~sexp:[%sexp (record : Record.t)]
;;

let sexp_files dir =
  match Sys_unix.readdir dir with
  | exception (_ : exn) -> []
  | files ->
    Array.to_list files
    |> List.filter ~f:(fun file -> String.is_suffix file ~suffix:".sexp")
    |> List.sort ~compare:String.compare
;;

let load_board ~runs_dir ~symbols ~date ~alpha_hash ~engine_name ~physics =
  let dir =
    board_dir ~runs_dir ~symbols ~date ~alpha_hash ~engine_name ~physics
  in
  sexp_files dir
  |> List.filter_map ~f:(fun file ->
    load_sexp_file (dir ^/ file) ~of_sexp:[%of_sexp: Record.t])
  |> List.map ~f:Record.row
  |> List.sort
       ~compare:
         (Comparable.lift
            (Comparable.reverse Int63.compare)
            ~f:(fun (row : Leaderboard_row.t) -> row.summary.value_add_cents))
;;

let save_user_run ~runs_dir ~username (run : Saved_run.t) =
  let file = user_run_file ~runs_dir ~username ~run_id:run.run_id in
  Core_unix.mkdir_p (user_dir ~runs_dir ~username);
  write_sexp_file file ~sexp:[%sexp (run : Saved_run.t)]
;;

let find_user_run ~runs_dir ~username ~run_id =
  load_sexp_file
    (user_run_file ~runs_dir ~username ~run_id)
    ~of_sexp:[%of_sexp: Saved_run.t]
;;

(* Newest first. [ran_at] is a fixed-width UTC timestamp, so string order is
   time order; the id breaks ties so a notebook listing is stable. *)
let newest_first (a : Saved_run.t) (b : Saved_run.t) =
  match String.compare b.ran_at a.ran_at with
  | 0 -> String.compare a.run_id b.run_id
  | ordering -> ordering
;;

let load_user_runs ~runs_dir ~username =
  let dir = user_dir ~runs_dir ~username in
  sexp_files dir
  |> List.filter_map ~f:(fun file ->
    load_sexp_file (dir ^/ file) ~of_sexp:[%of_sexp: Saved_run.t])
  |> List.sort ~compare:newest_first
;;

let delete_user_runs ~runs_dir ~username =
  let dir = user_dir ~runs_dir ~username in
  let files = sexp_files dir in
  List.iter files ~f:(fun file ->
    try Core_unix.unlink (dir ^/ file) with (_ : exn) -> ());
  List.length files
;;

let mark_published ~runs_dir ~username ~run_id =
  match find_user_run ~runs_dir ~username ~run_id with
  | None -> ()
  | Some run ->
    let published = { run with Saved_run.published = true } in
    write_sexp_file
      (user_run_file ~runs_dir ~username ~run_id)
      ~sexp:[%sexp (published : Saved_run.t)]
;;
