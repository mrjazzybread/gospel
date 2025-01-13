exception E of int

val f : int -> int
(*@ let y = f x in
    raises E *)

(* {gospel_expected|
   [125] File "exception_no_pattern.mli", line 5, characters 11-12:
         5 |     raises E *)
                        ^
         Error: Type checking error: Exception pattern does not match its type.
   |gospel_expected} *)
