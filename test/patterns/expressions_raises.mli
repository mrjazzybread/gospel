type t = A | B of int | C of int * int | D of (int * int)

exception E of int * int
exception F of (int * int)

val f : int -> t -> bool
(*@ requires t = D (n, n)
    requires t = C (n, n)
    requires let x = (n, n) in D x = D x
    (* but not: *)
    (* requires let x = (n, n) in C x = C x *)
    requires match t with
             | A
             | B _
             | C (_, _)
             | C _
             | D (_, _)
             | D ((_, _))
             | D _ -> true
    match f n t with
    | exception E -> ensures true
    (* but not: *)
    (* raises E _ -> false *)
    | exception F -> ensures true *)
