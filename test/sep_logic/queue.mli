(*@ open Sequence *)

type 'a t
(*@ mutable model view : 'a Sequence.t *)

        (*
val push : 'a t -> 'a -> unit
(*@ push q n
    ensures q = cons n (old q)
    modifies q *)

val pop : 'a t -> 'a
(*@ n = pop q
    requires q <> empty
    ensures old q = q ++ (singleton n)
    modifies q
 *)

val length : 'a t -> int
(*@ n = length q
    ensures n = length q *)
 *)
