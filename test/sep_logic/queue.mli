(*@ open Sequence *)

type t
(*@ mutable model view: int Sequence.t *)

val push : t -> int -> unit
(*@ push q n
    ensures q = cons n (old q)
    modifies q *)

val pop : t -> int 
(*@ n = pop q
    requires q <> empty
    ensures old q = q ++ (singleton n)
    modifies q
 *)

val length : t -> int
(*@ n = length q
    ensures n = length q *)
