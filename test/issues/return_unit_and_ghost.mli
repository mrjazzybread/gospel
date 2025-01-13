(* we would expect a nicer way to specify a function returning unit with a
   ghost value *)

val f : int -> unit
(*@ let [y : integer] = f x in
    ensures true *)

(* {gospel_expected|
   [125] File "return_unit_and_ghost.mli", line 5, characters 24-25:
         5 | (*@ let [y : integer] = f x in
                                     ^
         Error: Type checking error: too few returned values.
   |gospel_expected} *)
