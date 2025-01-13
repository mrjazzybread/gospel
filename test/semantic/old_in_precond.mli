val first : 'a array -> 'a
(*@ requires Sequence.length (old a) >= 1
    let x = first a *)

(* {gospel_expected|
   [125] File "old_in_precond.mli", line 2, characters 29-36:
         2 | (*@ requires Sequence.length (old a) >= 1
                                          ^^^^^^^
         Error: old operator is not allowed in requires clauses.
   |gospel_expected} *)
