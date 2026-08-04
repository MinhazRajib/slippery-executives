open! Core
open! Execlab_types
open! Execlab_market

let dates_for ~data_dir ~symbol =
  match Sys_unix.readdir (data_dir ^/ Symbol.to_string symbol) with
  | exception (_ : exn) -> []
  | files ->
    Array.to_list files
    |> List.filter_map ~f:(fun file ->
      let open Option.Let_syntax in
      let%bind date_string = String.chop_suffix file ~suffix:".csv" in
      Option.try_with (fun () -> Date.of_string date_string))
    |> List.sort ~compare:Date.compare
;;

let symbols ~data_dir =
  match Sys_unix.readdir data_dir with
  | exception (_ : exn) -> []
  | entries ->
    Array.to_list entries
    |> List.filter ~f:(fun entry -> not (String.equal entry "raw"))
    |> List.filter_map ~f:(fun entry ->
      Option.try_with (fun () -> Symbol.of_string entry))
    |> List.filter ~f:(fun symbol ->
      not (List.is_empty (dates_for ~data_dir ~symbol)))
    |> List.sort ~compare:Symbol.compare
;;

let load ~data_dir ~symbol ~date =
  Data_loader.load ~data_dir ~symbol ~date ()
;;

let forecast_days ~data_dir ~symbol ~excluding =
  dates_for ~data_dir ~symbol
  |> List.filter ~f:(fun date -> not (Date.equal date excluding))
  |> List.filter_map ~f:(fun date ->
    Or_error.ok (load ~data_dir ~symbol ~date))
;;
