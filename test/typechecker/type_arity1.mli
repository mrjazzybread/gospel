type ('a, 'b) t
(*@ model m : ('a, 'b) list *)

(* {gospel_expected|
   [125] File "type_arity1.mli", line 2, characters 23-27:
         2 | (*@ model m : ('a, 'b) list *)
                                    ^^^^
         Error: The type list expects 1 argument(s) but was given 2 argument(s) here.
   |gospel_expected} *)
