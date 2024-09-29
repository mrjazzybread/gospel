type t
(*@ model x : int
    model y : int *)

val f : t -> t
(*@ r = f arg
    ensures arg.x = r.y *)
