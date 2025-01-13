type t = C of int * int

val f : int -> int
(*@ requires C (n, n, n) = C (n, n)
    let m = f n *)

(* {gospel_expected|
   [125] File "constructor_arity5.mli", line 4, characters 13-14:
         4 | (*@ requires C (n, n, n) = C (n, n)
                          ^
         Error: The constructor C expects 2 argument(s)
                but is applied to 3 argument(s) here.
   |gospel_expected} *)
