val f : int -> int
(*@ requires match x with
    let y = f x *)

(* {gospel_expected|
   [125] File "empty_match.mli", line 3, characters 4-7:
         3 |     let y = f x *)
                 ^^^
         Error: Syntax error.
   |gospel_expected} *)
