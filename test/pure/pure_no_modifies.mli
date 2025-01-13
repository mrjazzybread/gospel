type t
(*@ mutable model view: int *)

val f : t -> int
(*@ pure
    modifies x
    let y = f x *)

(* {gospel_expected|
   [125] File "pure_no_modifies.mli", line 7, characters 12-13:
         7 |     let y = f x *)
                         ^
         Error: Type checking error: a pure function cannot have writes.
   |gospel_expected} *)
