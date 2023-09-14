(*@ open Sequence *)

type t
(*@ mutable model view: int Sequence.t *)

val push : t -> int -> unit
(*@ push q n
    ensures q.view = cons n (old q.view)
    modifies q *)

val pop : t -> int 
(*@ n = pop q
    requires q.view <> empty
    ensures q.view = (old q.view) ++ (singleton n)
    modifies q
 *)

val length : t -> int
(*@ n = length q
    ensures n = length q.view *)
