val f : int -> int
(*@ let y = f x in
    ensures match x with _ when true -> true
*)

(* {gospel_expected|
   [125] File "not_all_guarded2.mli", line 3, characters 12-44:
         3 |     ensures match x with _ when true -> true
                         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
         Error: All clauses in this pattern-matching are guarded.
   |gospel_expected} *)
