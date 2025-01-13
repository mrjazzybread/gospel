type t = C of int * int

val f : int -> t -> unit
(*@ requires let x = (n, n) in C x = C x
    let _ = f n t *)

(* {gospel_expected|
   [125] File "constructor_arity1.mli", line 4, characters 31-32:
         4 | (*@ requires let x = (n, n) in C x = C x
                                            ^
         Error: The symbol C cannot be partially applied.
   |gospel_expected} *)
