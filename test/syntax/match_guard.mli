val f : int -> int
(*@ requires 0 = match x with
           | _ when false->true -> 1
    let r = f x
*)

(* read as [false -> (true -> 1)] *)
(* There is no simple way to disambiguate between the match-case [->] and
   implies *)

(* {gospel_expected|
   [125] File "match_guard.mli", line 3, characters 35-36:
         3 |            | _ when false->true -> 1
                                                ^
         Error: A formula was expected.
   |gospel_expected} *)
