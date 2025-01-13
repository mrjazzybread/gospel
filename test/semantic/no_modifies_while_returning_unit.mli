val no_modifies_while_returning_unit : int -> unit
(*@ requires true
    let _ = no_modifies_while_returning_unit i *)

(* {gospel_expected|
   [125] File "no_modifies_while_returning_unit.mli", line 1, characters 0-118:
         1 | val no_modifies_while_returning_unit : int -> unit
         2 | (*@ requires true
         3 |     let _ = no_modifies_while_returning_unit i *)
         Error: The function no_modifies_while_returning_unit returns unit
                but its specifications does not contain any modifies clause.
   |gospel_expected} *)
