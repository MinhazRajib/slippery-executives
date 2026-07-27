open! Core
open! Execlab_types
open Execlab_market

let create ~open_ ~high ~low ~close ~volume =
  Market_bar.create
    ~time:(Time_ns.Ofday.of_string "09:30:00")
    ~open_:(Price.of_float_round_nearest open_)
    ~high:(Price.of_float_round_nearest high)
    ~low:(Price.of_float_round_nearest low)
    ~close:(Price.of_float_round_nearest close)
    ~volume:(Size.of_int volume)
;;

let print result = print_s [%sexp (result : Market_bar.t Or_error.t)]

let%expect_test "create accepts a real bar" =
  (* First regular-session bar of data/TSLA/2026-07-09.csv; sub-cent prices
     round to the nearest cent on load. *)
  print
    (create
       ~open_:393.99
       ~high:395.538
       ~low:392.21
       ~close:394.43
       ~volume:391608);
  [%expect
    {|
    (Ok
     ((time 09:30:00.000000000) (open_ 39399) (high 39554) (low 39221)
      (close 39443) (volume 391608)))
    |}]
;;

let%expect_test "high must not be below low" =
  print (create ~open_:393. ~high:392. ~low:393. ~close:393. ~volume:100);
  [%expect
    {|
    (Error
     ("High price must be greater than or equal to low price" (high 39200)
      (low 39300)))
    |}]
;;

let%expect_test "open must lie within the bar's range" =
  print (create ~open_:396. ~high:395. ~low:394. ~close:394.5 ~volume:100);
  [%expect
    {|
    (Error
     ("Open price must be between low and high prices" (open_ 39600) (low 39400)
      (high 39500)))
    |}]
;;

let%expect_test "close must lie within the bar's range" =
  print (create ~open_:394.5 ~high:395. ~low:394. ~close:393. ~volume:100);
  [%expect
    {|
    (Error
     ("Close price must be between low and high prices" (close 39300) (low 39400)
      (high 39500)))
    |}]
;;

let%expect_test "volume must be non-negative" =
  print
    (create ~open_:394.5 ~high:395. ~low:394. ~close:394.5 ~volume:(-100));
  [%expect {| (Error ("Volume must be non-negative" (volume -100))) |}]
;;

let%expect_test "zero volume is allowed (halted or quiet minutes)" =
  print (create ~open_:394.5 ~high:395. ~low:394. ~close:394.5 ~volume:0);
  [%expect
    {|
    (Ok
     ((time 09:30:00.000000000) (open_ 39450) (high 39500) (low 39400)
      (close 39450) (volume 0)))
    |}]
;;
