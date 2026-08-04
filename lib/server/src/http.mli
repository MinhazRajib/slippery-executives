(** A deliberately tiny blocking HTTP/1.1 server: enough to serve the static
    client and carry sexp API bodies, and nothing more. One connection at a
    time — the whole user base is two people on one VM, and sequential means
    no locks. No TLS, no keep-alive, no chunked encoding; the day any of that
    matters, this module is replaced wholesale, not extended. *)

open! Core

module Request : sig
  type t =
    { meth : string (** ["GET"], ["POST"], ... *)
    ; path : string (** as sent, query string included *)
    ; body : string
    }
  [@@deriving sexp_of]
end

val respond
  :  Out_channel.t
  -> status:string (** e.g. ["200 OK"] *)
  -> content_type:string
  -> string
  -> unit

val content_type_of_path : string -> string

(** [path] with the query stripped and normalized to relative segments;
    [None] if it tries to escape upward ([".."]). *)
val safe_relative_path : string -> string option

(** Accept loop; never returns. [handle] must write exactly one response. *)
val serve_forever
  :  port:int
  -> handle:(request:Request.t -> writer:Out_channel.t -> unit)
  -> never_returns
