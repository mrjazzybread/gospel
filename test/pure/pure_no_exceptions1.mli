val f : int -> int
(*@ pure
    let y = f x in
    raises Not_found -> true *)

(* {gospel_expected|
   [125] File "pure_no_exceptions1.mli", line 3, characters 12-13:
         3 |     let y = f x in
                         ^
         Error: Type checking error: a pure function cannot raise exceptions.
   |gospel_expected} *)
