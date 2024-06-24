(*@ open Sequence *)

type t
(*@ mutable model : int Sequence.t *)

val pop : t -> int
(*@ r = pop q
    modifies q
    ensures q = tl (old q)  *)
