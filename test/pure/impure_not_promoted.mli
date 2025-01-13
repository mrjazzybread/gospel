val f : int -> int
val g : int -> int
(*@ requires f x > 0
    let y = g x
*)

(* An impure OCaml function is not promoted to a Gospel function *)

(* {gospel_expected|
   [125] File "impure_not_promoted.mli", line 3, characters 13-14:
         3 | (*@ requires f x > 0
                          ^
         Error: Symbol f not found in scope
                (see "Symbols in scope" documentation page).
   |gospel_expected} *)
