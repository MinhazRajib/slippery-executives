open! Core
open! Execlab_types

let expected_header = "time,open,high,low,close,volume"

let parse_bar ~line_number line =
  Or_error.tag_s
    (Or_error.try_with (fun () ->
       match String.split line ~on:',' with
       | [ time; open_; high; low; close; volume ] ->
         let price field =
           Price.of_float_round_nearest
             (Float.of_string (String.strip field))
         in
         Or_error.ok_exn
           (Market_bar.create
              ~time:(Time_ns.Ofday.of_string (String.strip time))
              ~open_:(price open_)
              ~high:(price high)
              ~low:(price low)
              ~close:(price close)
              ~volume:(Size.of_int (Int.of_string (String.strip volume))))
       | fields ->
         raise_s
           [%message
             "expected 6 comma-separated fields" (List.length fields : int)]))
    ~tag:[%message "Invalid bar row" (line_number : int) (line : string)]
;;

let parse ~symbol ~date contents =
  match String.split_lines (String.strip contents) with
  | [] -> Or_error.error_s [%message "Market data file is empty"]
  | header :: rows ->
    if not (String.equal (String.strip header) expected_header)
    then
      Or_error.error_s
        [%message
          "Unexpected header" (header : string) (expected_header : string)]
    else
      let open Or_error.Let_syntax in
      let%bind bars =
        List.mapi rows ~f:(fun i line -> parse_bar ~line_number:(i + 2) line)
        |> Or_error.combine_errors
      in
      Trading_day.create ~symbol ~date ~bars
;;

let load ?(data_dir = "data") ~symbol ~date () =
  let filename = [%string "%{data_dir}/%{symbol#Symbol}/%{date#Date}.csv"] in
  match In_channel.read_all filename with
  | exception Sys_error message ->
    Or_error.error_s
      [%message
        "Could not read market data file"
          (filename : string)
          (message : string)]
  | contents -> parse ~symbol ~date contents
;;
