(*@ open Sequence *)

type t
(*@ mutable model view: int Sequence.t *)

val push : t -> int -> unit
(*@ push q n
    requires true
    ensures q.view = cons n (old q.view)
    modifies q *)

val pop : t -> int 
(*@ n = pop q
    requires q.view <> empty
    ensures q.view = (old q.view) ++ (singleton n)
    modifies q
*)