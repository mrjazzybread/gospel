type t = { f : int }

val f : int -> int
(*@ pure *)

val g : int -> int
(*@ requires f x > 0
    let y = g x *)
