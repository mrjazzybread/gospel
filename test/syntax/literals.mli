(** Tests for constant literals *)

val f : int -> float
(*@ requires x = 0
    let y = f x in
    ensures y = 0. *)

val g : char -> string
(*@ requires x = 'c'
    let y = g x in
    ensures y = "c" *)
