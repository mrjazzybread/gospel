type t = A

val f : t -> int
(*@ requires match x with
           | A when 1 = 1 -> true
           | _ when true  -> false
    let r = f x
*)

(* {gospel_expected|
   [125] File "not_all_guarded1.mli", line 4, characters 13-94:
         4 | .............match x with
         5 |            | A when 1 = 1 -> true
         6 |            | _ when true  -> false
         Error: All clauses in this pattern-matching are guarded.
   |gospel_expected} *)
