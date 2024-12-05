exception E of int * int

val f : int -> unit
(*@ match f i with
    | exception E x -> ensures false *)

(* {gospel_expected|
   [125] Error: Type checking error: Exception pattern has 1 arguments but expected 2.
   |gospel_expected} *)
