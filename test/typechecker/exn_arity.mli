exception E of int * int

val f : int -> unit
(*@ f i
    raises E x -> false *)

(* {gospel_expected|
   [125] File "exn_arity.mli", line 5, characters 11-23:
         5 |     raises E x -> false *)
                        ^^^^^^^^^^^^
         Error: Type checking error: Exception pattern has 1 arguments but expected 2.
   |gospel_expected} *)
