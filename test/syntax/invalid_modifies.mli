val x : int -> int
val invalid_modifies : unit -> unit
(*@ modifies x *)

(* Only current function's inputs are in scope for modifies clauses.
   TODO refine error message *)

(* {gospel_expected|
   [125] File "invalid_modifies.mli", line 3, characters 13-14:
         3 | (*@ modifies x *)
                          ^
         Error: Symbol x not found in scope
                (see "Symbols in scope" documentation page).
   |gospel_expected} *)
