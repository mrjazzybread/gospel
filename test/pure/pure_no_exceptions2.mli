val f : int -> int
(*@ pure
    checks x > 0
    let y = f x *)

(* {gospel_expected|
   [125] File "pure_no_exceptions2.mli", line 4, characters 12-13:
         4 |     let y = f x *)
                         ^
         Error: Type checking error: a pure function cannot raise exceptions.
   |gospel_expected} *)
