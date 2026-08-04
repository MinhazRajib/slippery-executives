open! Core
open! Execlab_types

type t =
  { bids : (int * int) list
  ; asks : (int * int) list
  }
[@@deriving sexp_of]

let empty = { bids = []; asks = [] }

let sort_side ~side levels =
  let levels =
    List.filter levels ~f:(fun ((_ : int), size) -> size > 0)
    |> List.sort_and_group ~compare:(fun (p1, (_ : int)) (p2, (_ : int)) ->
      Int.compare p1 p2)
    |> List.map ~f:(fun group ->
      let price, (_ : int) = List.hd_exn group in
      price, List.sum (module Int) group ~f:snd)
  in
  match (side : Side.t) with
  | Buy -> List.rev levels (* best bid = highest, first *)
  | Sell -> levels (* best ask = lowest, first *)
;;

let set_side t ~side quotes =
  let levels =
    sort_side
      ~side
      (List.map quotes ~f:(fun (price, size) ->
         Price.to_int_cents price, size))
  in
  match (side : Side.t) with
  | Buy -> { t with bids = levels }
  | Sell -> { t with asks = levels }
;;

let best t ~side =
  let levels = match (side : Side.t) with Buy -> t.bids | Sell -> t.asks in
  Option.map (List.hd levels) ~f:(fun (price, (_ : int)) ->
    Price.of_int_cents price)
;;

(* Whether a taker at [limit] may trade a maker level at [price]. *)
let crosses ~taker_side ~limit price =
  match limit with
  | None -> true
  | Some limit ->
    let limit = Price.to_int_cents limit in
    (match (taker_side : Side.t) with
     | Buy -> price <= limit
     | Sell -> price >= limit)
;;

let take t ~taker_side ?limit ~size () =
  let levels =
    match (taker_side : Side.t) with
    | Buy -> t.asks (* a buyer lifts asks *)
    | Sell -> t.bids
  in
  let rec walk levels size fills =
    match levels with
    | [] -> levels, size, fills
    | (price, available) :: deeper as untouched ->
      if size <= 0 || not (crosses ~taker_side ~limit price)
      then untouched, size, fills
      else if available <= size
      then walk deeper (size - available) ((price, available) :: fills)
      else (price, available - size) :: deeper, 0, (price, size) :: fills
  in
  let levels, (_ : int), fills = walk levels size [] in
  let fills =
    List.rev_map fills ~f:(fun (price, size) ->
      Price.of_int_cents price, size)
  in
  match (taker_side : Side.t) with
  | Buy -> { t with asks = levels }, fills
  | Sell -> { t with bids = levels }, fills
;;
