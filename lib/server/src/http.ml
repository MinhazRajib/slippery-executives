open! Core

module Request = struct
  type t =
    { meth : string
    ; path : string
    ; body : string
    }
  [@@deriving sexp_of]
end

(* Reads one request from [reader]: request line, headers to the blank line,
   then exactly Content-Length bytes of body. Returns [None] on a closed or
   malformed connection — the caller just drops it. *)
(* Bodies beyond this are dropped as malformed: nothing in the protocol is
   remotely this large, and Bytes.create of an attacker-chosen (or negative)
   Content-Length must never run. *)
let max_body_bytes = 8 * 1024 * 1024

let read_request reader =
  let open Option.Let_syntax in
  let%bind request_line = In_channel.input_line reader in
  let%bind meth, path =
    match String.split request_line ~on:' ' with
    | meth :: path :: (_ : string list) -> Some (meth, path)
    | _ -> None
  in
  let rec headers length =
    match In_channel.input_line reader with
    | None -> None
    | Some line ->
      let line = String.rstrip line in
      if String.is_empty line
      then Some length
      else (
        match String.lsplit2 line ~on:':' with
        | Some (name, value)
          when String.Caseless.equal (String.strip name) "content-length" ->
          headers (Int.of_string_opt (String.strip value))
        | Some (_ : string * string) | None -> headers length)
  in
  let%bind length = headers None in
  let%bind length =
    match length with
    | None -> Some None
    | Some length when length >= 0 && length <= max_body_bytes ->
      Some (Some length)
    | Some (_ : int) -> None
  in
  let%map body =
    match length with
    | None -> Some ""
    | Some length ->
      let buffer = Bytes.create length in
      (match
         In_channel.really_input reader ~buf:buffer ~pos:0 ~len:length
       with
       | Some () -> Some (Bytes.to_string buffer)
       | None -> None)
  in
  { Request.meth; path; body }
;;

let respond writer ~status ~content_type body =
  Out_channel.output_string
    writer
    (sprintf
       "HTTP/1.1 %s\r\n\
        Content-Type: %s\r\n\
        Content-Length: %d\r\n\
        Cache-Control: no-cache\r\n\
        Connection: close\r\n\
        \r\n"
       status
       content_type
       (String.length body));
  Out_channel.output_string writer body;
  Out_channel.flush writer
;;

let content_type_of_path path =
  match snd (Filename.split_extension path) with
  | Some "html" -> "text/html; charset=utf-8"
  | Some "js" -> "application/javascript"
  | Some "css" -> "text/css"
  | Some "csv" -> "text/csv"
  | Some "json" -> "application/json"
  | Some (_ : string) | None -> "application/octet-stream"
;;

(* Path sanitization: no [..] segments ever reach the filesystem. *)
let safe_relative_path path =
  let path =
    match String.lsplit2 path ~on:'?' with
    | Some (path, (_ : string)) -> path
    | None -> path
  in
  let segments =
    String.split path ~on:'/'
    |> List.filter ~f:(fun segment ->
      (not (String.is_empty segment)) && not (String.equal segment "."))
  in
  if List.exists segments ~f:(String.equal "..")
  then None
  else Some (String.concat ~sep:"/" segments)
;;

let serve_forever ~port ~handle =
  let socket =
    Core_unix.socket ~domain:PF_INET ~kind:SOCK_STREAM ~protocol:0 ()
  in
  Core_unix.setsockopt socket SO_REUSEADDR true;
  Core_unix.bind
    socket
    ~addr:(ADDR_INET (Core_unix.Inet_addr.bind_any, port));
  Core_unix.listen socket ~backlog:16;
  (* A client vanishing mid-response must be a dropped connection, not a
     fatal SIGPIPE to the whole process. Core's [Signal] has no handler
     installer outside Async, so this goes through Stdlib — the server is
     single-domain, which is what the multidomain alert is about. *)
  Stdlib.Sys.set_signal
    Stdlib.Sys.sigpipe
    Stdlib.Sys.Signal_ignore [@alert "-unsafe_multidomain"];
  eprintf "execlab server listening on http://localhost:%d\n%!" port;
  (* One connection at a time: two players and a demo do not need more, and
     sequential means no locking anywhere. *)
  let rec loop () =
    let client, (_ : Core_unix.sockaddr) = Core_unix.accept socket in
    (try
       (* The loop is sequential, so a silent client must not wedge it: reads
          and writes give up after ten seconds. *)
       Core_unix.setsockopt_float client SO_RCVTIMEO 10.;
       Core_unix.setsockopt_float client SO_SNDTIMEO 10.;
       let reader = Core_unix.in_channel_of_descr client in
       let writer = Core_unix.out_channel_of_descr client in
       (match read_request reader with
        | None -> ()
        | Some request -> handle ~request ~writer);
       Out_channel.flush writer
     with
     | (_ : exn) -> ());
    (try Core_unix.close client with (_ : exn) -> ());
    loop ()
  in
  loop ()
;;
