(** The account store: one sexp file per account under [accounts_dir],
    holding the username, a digest of the passcode, and the creation time.
    {!Execlab_protocol.Credentials} go in, an {!Execlab_protocol.Session.t}
    comes out, and every later authenticated call arrives as a token that
    {!username_of_token} resolves back to an account. {!Store} keeps that
    directory at [runs_dir ^/ "accounts"] and {!Service} is the only caller.

    {2 What the token is, honestly}

    The token is
    [username ^ "." ^ md5 ("execlab-tok:" ^ username ^ ":" ^ passcode_digest)]
    — a pure function of the stored account. There is no token database,
    which is why tokens survive a server restart and why they never expire:
    possession of one is exactly as powerful as knowing the passcode,
    forever, and it cannot be revoked short of deleting the account.
    Passcodes are digested with md5 and no salt or stretching, so the file is
    not safe to leak either.

    This is a bearer credential attached to a scoreboard, not authentication.
    It exists so a run can carry an owner across devices; it defends against
    a bored player typing someone else's name into a config, and against
    nothing stronger. Do not put anything behind it that would hurt to lose.

    {[
      let%bind session = Accounts.create ~accounts_dir ~username ~passcode in
      let%bind username = Accounts.username_of_token ~accounts_dir ~token:session.token in
      ...
    ]} *)

open! Core
open Execlab_protocol

(** Registers a new account. Errors if [username] is taken, if it is not 3 to
    24 characters of [[a-zA-Z0-9_-]], or if [passcode] is shorter than 4
    characters. Usernames are case-sensitive: ["Ada"] and ["ada"] are two
    accounts. *)
val create
  :  accounts_dir:string
  -> username:string
  -> passcode:string
  -> Session.t Or_error.t

(** Reissues the session for an existing account. The error deliberately does
    not say whether the account is unknown or the passcode is wrong — the
    board is public, so account existence should not be. *)
val sign_in
  :  accounts_dir:string
  -> username:string
  -> passcode:string
  -> Session.t Or_error.t

(** The account a token belongs to: splits on the first ['.'], loads that
    account, recomputes its expected token and compares. Errors — uniformly,
    whatever went wrong — if the shape is wrong, the account is gone, or the
    digest does not match. *)
val username_of_token
  :  accounts_dir:string
  -> token:string
  -> string Or_error.t

(** True for strings that are safe to use as one filesystem path component:
    non-empty and drawn from [[a-zA-Z0-9_-]], so no separator, no [".."], no
    leading dot. Every username accepted by {!create} satisfies it; this is
    exported so {!Store} can re-assert it before building a per-account
    directory rather than trust that the value came from a token. *)
val is_filename_safe : string -> bool
