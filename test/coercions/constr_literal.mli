(*@ type t = A of integer *)

val f : int -> int
(*@ requires A 42i = A 42i
    let y = f x *)
