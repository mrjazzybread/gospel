val f : int -> int
(*@ requires match x with
           | 1i -> true
           | _ when 1 = 2 -> false
    let r = f x
*)

(* {gospel_expected|
   [125] File "nonexhaustive_guard.mli", line 2, characters 13-84:
         2 | .............match x with
         3 |            | 1i -> true
         4 |            | _ when 1 = 2 -> false
         Error: This pattern-matching may not be exhaustive because of the guard.
                Here is an example of a case that may not be matched:
                  0i.
   |gospel_expected} *)
