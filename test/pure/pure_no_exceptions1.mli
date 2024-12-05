val f : int -> int
(*@ pure
    match f x with
    |exception Not_found *)

(* {gospel_expected|
   [125] File "pure_no_exceptions1.mli", line 3, characters 10-11:
         3 |     match f x with
                       ^
         Error: Type checking error: a pure function cannot raise exceptions.
   |gospel_expected} *)
