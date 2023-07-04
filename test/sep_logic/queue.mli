type t
(*@ mutable model view: int list *)

val push : t -> int -> unit
(*@ push q n
    ensures q.view = old(n::q.view)
    modifies q *)