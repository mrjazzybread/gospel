(*@ type m = A of unit | B of (unit * unit) *)

type t
(*@ mutable model m : m *)

val f : t -> bool
(*@ let b = f t in
      ensures match t.m with | A _ -> true | B _ -> false
*)
(* {gospel_expected|
   [125] File "pattern_analysis_tuple.mli", line 8, characters 14-57:
         8 |       ensures match t.m with | A _ -> true | B _ -> false
                           ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
         Error: The pattern-matching is redundant.
                Here is a case that is unused:
                  B _.
   |gospel_expected} *)
