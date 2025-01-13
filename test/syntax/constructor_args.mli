type t = C of int * int

val f : int -> int
(*@ requires C (n, n) = C n n
    let m = f n
*)

(* {gospel_expected|
   [125] File "constructor_args.mli", line 4, characters 24-25:
         4 | (*@ requires C (n, n) = C n n
                                     ^
         Error: Syntax error.
   |gospel_expected} *)
