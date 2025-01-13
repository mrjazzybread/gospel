val f : int -> int
(*@ pure
    diverges *)

(* {gospel_expected|
   [125] File "pure_no_diverges.mli", line 1, characters 0-43:
         1 | val f : int -> int
         2 | (*@ pure
         3 |     diverges *)
         Error: Type checking error: a pure function cannot diverge.
   |gospel_expected} *)
