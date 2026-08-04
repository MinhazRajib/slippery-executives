open! Core
open Execlab_protocol

let min_username_length = 3
let max_username_length = 24
let min_passcode_length = 4

(* Every failure to produce a session says the same thing. Which half of the
   pair was wrong is exactly what an attacker wants to learn, and a player
   who mistyped their own name is no worse off for the vagueness. *)
let sign_in_failed = "unknown account or wrong passcode"
let bad_token = "invalid session token; sign in again"

module Account = struct
  (* On-disk shape. The passcode itself is never stored; [passcode_hash] is
     also the seed of the account's token, so the file is enough to
     reconstruct a session and must not be served (see {!Service}). *)
  type t =
    { username : string
    ; passcode_hash : string
    ; created_at : string
    }
  [@@deriving sexp]
end

let is_legal_username_char c =
  Char.is_alphanum c || Char.equal c '_' || Char.equal c '-'
;;

let is_filename_safe name =
  (not (String.is_empty name))
  && String.for_all name ~f:is_legal_username_char
;;

let validate_username username =
  let length = String.length username in
  match
    length >= min_username_length
    && length <= max_username_length
    && is_filename_safe username
  with
  | true -> Ok ()
  | false ->
    Or_error.error_s
      [%message
        "username must be 3-24 chars of [a-zA-Z0-9_-]" (username : string)]
;;

(* The passcode never appears in an error: these messages reach the client. *)
let validate_passcode passcode =
  match String.length passcode >= min_passcode_length with
  | true -> Ok ()
  | false ->
    Or_error.error_s [%message "passcode must be at least 4 characters"]
;;

let account_file ~accounts_dir ~username =
  (* [validate_username]'s charset already rules out '/' and "..", but this
     is the line where a username becomes a path, so it states the invariant
     instead of assuming every caller upstream got it right. *)
  if not (is_filename_safe username)
  then
    raise_s
      [%message "username is not usable as a filename" (username : string)];
  accounts_dir ^/ username ^ ".sexp"
;;

let passcode_hash ~username ~passcode =
  Md5.to_hex
    (Md5.digest_string [%string "execlab-pc:%{username}:%{passcode}"])
;;

let token_of_account (account : Account.t) =
  let digest =
    Md5.to_hex
      (Md5.digest_string
         [%string "execlab-tok:%{account.username}:%{account.passcode_hash}"])
  in
  [%string "%{account.username}.%{digest}"]
;;

let session_of_account (account : Account.t) =
  { Session.username = account.username; token = token_of_account account }
;;

let find_account ~accounts_dir ~username =
  Option.try_with (fun () ->
    [%of_sexp: Account.t]
      (Sexp.load_sexp (account_file ~accounts_dir ~username)))
;;

let now_string () =
  Time_ns.to_string_utc (Time_ns.now ())
  |> String.tr ~target:' ' ~replacement:'T'
;;

let create ~accounts_dir ~username ~passcode =
  let open Or_error.Let_syntax in
  let%bind () = validate_username username in
  let%bind () = validate_passcode passcode in
  let file = account_file ~accounts_dir ~username in
  match Sys_unix.file_exists file with
  | `Yes ->
    Or_error.error_s
      [%message "username is already taken" (username : string)]
  | `Unknown ->
    Or_error.error_s
      [%message
        "cannot tell whether that account exists" (username : string)]
  | `No ->
    let account =
      { Account.username
      ; passcode_hash = passcode_hash ~username ~passcode
      ; created_at = now_string ()
      }
    in
    Or_error.try_with (fun () ->
      Core_unix.mkdir_p accounts_dir;
      Out_channel.write_all
        file
        ~data:(Sexp.to_string_hum [%sexp (account : Account.t)]);
      session_of_account account)
;;

let sign_in ~accounts_dir ~username ~passcode =
  (* A malformed username cannot name an existing account, so it takes the
     same exit as a wrong passcode — and never reaches [account_file]. *)
  let failed () = Or_error.error_string sign_in_failed in
  match
    Or_error.both (validate_username username) (validate_passcode passcode)
  with
  | Error (_ : Error.t) -> failed ()
  | Ok ((), ()) ->
    (match find_account ~accounts_dir ~username with
     | None -> failed ()
     | Some account ->
       (match
          String.equal
            account.passcode_hash
            (passcode_hash ~username ~passcode)
        with
        | false -> failed ()
        | true -> Ok (session_of_account account)))
;;

let username_of_token ~accounts_dir ~token =
  let failed () = Or_error.error_string bad_token in
  match String.lsplit2 token ~on:'.' with
  | None -> failed ()
  | Some (username, (_ : string)) ->
    (match validate_username username with
     | Error (_ : Error.t) -> failed ()
     | Ok () ->
       (match find_account ~accounts_dir ~username with
        | None -> failed ()
        | Some account ->
          (match String.equal token (token_of_account account) with
           | false -> failed ()
           | true -> Ok username)))
;;
