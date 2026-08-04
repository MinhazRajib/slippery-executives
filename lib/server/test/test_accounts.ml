open! Core
open Execlab_protocol
open Execlab_server

let entries dir =
  match Sys_unix.readdir dir with
  | exception (_ : exn) -> []
  | files -> Array.to_list files |> List.sort ~compare:String.compare
;;

let rec remove_tree path =
  match Sys_unix.is_directory path with
  | `Yes ->
    List.iter (entries path) ~f:(fun entry -> remove_tree (path ^/ entry));
    Core_unix.rmdir path
  | `No | `Unknown -> Core_unix.unlink path
;;

(* The accounts directory sits one level inside the temp root, so a test can
   show that a hostile username lands nowhere at all — neither in the
   accounts directory nor beside it. Only basenames are ever printed: the
   temp path itself changes every run. *)
let with_accounts_dir ~f =
  let root = Filename_unix.temp_dir "execlab-accounts" "" in
  Exn.protect
    ~f:(fun () -> f ~root ~accounts_dir:(root ^/ "accounts"))
    ~finally:(fun () -> remove_tree root)
;;

let print_session (result : Session.t Or_error.t) =
  match result with
  | Error error -> print_s [%sexp (error : Error.t)]
  | Ok session ->
    print_endline [%string "ok %{session.username} %{session.token}"]
;;

let print_username (result : string Or_error.t) =
  match result with
  | Error error -> print_s [%sexp (error : Error.t)]
  | Ok username -> print_endline [%string "ok %{username}"]
;;

let print_entries dir = print_s [%sexp (entries dir : string list)]

(* The token is a pure function of the username and the passcode digest, so
   these two lines are computable off-line:

   {v
     md5 "execlab-pc:qasim:hunter2"       = 4db8d0cf5d39adfc019f9ed3c773cce7
     md5 "execlab-tok:qasim:4db8d0...ce7" = 868a23bc3a2326420866ccac6b874d19
   v} *)
let%expect_test "creating an account and signing in yield the same session" =
  with_accounts_dir ~f:(fun ~root:(_ : string) ~accounts_dir ->
    print_session
      (Accounts.create ~accounts_dir ~username:"qasim" ~passcode:"hunter2");
    print_session
      (Accounts.sign_in ~accounts_dir ~username:"qasim" ~passcode:"hunter2");
    print_entries accounts_dir);
  [%expect
    {|
    ok qasim qasim.868a23bc3a2326420866ccac6b874d19
    ok qasim qasim.868a23bc3a2326420866ccac6b874d19
    (qasim.sexp)
    |}]
;;

let%expect_test "a taken username cannot be claimed twice" =
  with_accounts_dir ~f:(fun ~root:(_ : string) ~accounts_dir ->
    print_session
      (Accounts.create ~accounts_dir ~username:"qasim" ~passcode:"hunter2");
    print_session
      (Accounts.create ~accounts_dir ~username:"qasim" ~passcode:"stolen!");
    (* The failed claim left the original account untouched. *)
    print_session
      (Accounts.sign_in ~accounts_dir ~username:"qasim" ~passcode:"hunter2"));
  [%expect
    {|
    ok qasim qasim.868a23bc3a2326420866ccac6b874d19
    ("username is already taken" (username qasim))
    ok qasim qasim.868a23bc3a2326420866ccac6b874d19
    |}]
;;

let%expect_test "a failed sign-in does not say which half was wrong" =
  with_accounts_dir ~f:(fun ~root:(_ : string) ~accounts_dir ->
    print_session
      (Accounts.create
         ~accounts_dir
         ~username:"mina"
         ~passcode:"correct-horse");
    print_session
      (Accounts.sign_in
         ~accounts_dir
         ~username:"mina"
         ~passcode:"wrong-horse");
    print_session
      (Accounts.sign_in
         ~accounts_dir
         ~username:"ghost"
         ~passcode:"correct-horse"));
  [%expect
    {|
    ok mina mina.c8d708573d74819468647d24f3a7e672
    "unknown account or wrong passcode"
    "unknown account or wrong passcode"
    |}]
;;

let%expect_test "a token resolves to its account; tampering does not" =
  with_accounts_dir ~f:(fun ~root:(_ : string) ~accounts_dir ->
    let token =
      match
        Accounts.create ~accounts_dir ~username:"qasim" ~passcode:"hunter2"
      with
      | Error error -> raise_s [%sexp (error : Error.t)]
      | Ok session -> session.token
    in
    print_username (Accounts.username_of_token ~accounts_dir ~token);
    (* Last hex digit flipped: the digest no longer matches the account. *)
    print_username
      (Accounts.username_of_token
         ~accounts_dir
         ~token:(String.drop_suffix token 1 ^ "0"));
    (* Right shape, no such account. *)
    print_username
      (Accounts.username_of_token
         ~accounts_dir
         ~token:("ghost" ^ String.drop_prefix token 5));
    print_username (Accounts.username_of_token ~accounts_dir ~token:"qasim");
    (* A traversal in the username half must not reach the filesystem. *)
    print_username
      (Accounts.username_of_token
         ~accounts_dir
         ~token:"../../etc/passwd.deadbeef"));
  [%expect
    {|
    ok qasim
    "invalid session token; sign in again"
    "invalid session token; sign in again"
    "invalid session token; sign in again"
    "invalid session token; sign in again"
    |}]
;;

let%expect_test "malformed credentials never reach the filesystem" =
  with_accounts_dir ~f:(fun ~root ~accounts_dir ->
    let create ~username ~passcode =
      print_session (Accounts.create ~accounts_dir ~username ~passcode)
    in
    create ~username:"ab" ~passcode:"hunter2";
    create ~username:(String.make 25 'a') ~passcode:"hunter2";
    create ~username:"../evil" ~passcode:"hunter2";
    create ~username:"qa sim" ~passcode:"hunter2";
    create ~username:"ok_name" ~passcode:"";
    create ~username:"ok_name" ~passcode:"abc";
    (* The whole legal charset, at both length ends of the range. *)
    create ~username:"a-b_C9" ~passcode:"hunter2";
    print_entries root;
    print_entries accounts_dir);
  [%expect
    {|
    ("username must be 3-24 chars of [a-zA-Z0-9_-]" (username ab))
    ("username must be 3-24 chars of [a-zA-Z0-9_-]"
     (username aaaaaaaaaaaaaaaaaaaaaaaaa))
    ("username must be 3-24 chars of [a-zA-Z0-9_-]" (username ../evil))
    ("username must be 3-24 chars of [a-zA-Z0-9_-]" (username "qa sim"))
    "passcode must be at least 4 characters"
    "passcode must be at least 4 characters"
    ok a-b_C9 a-b_C9.a80d337d773448a70863c69f6b76b801
    (accounts)
    (a-b_C9.sexp)
    |}]
;;
