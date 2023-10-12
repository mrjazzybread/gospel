(*@ open Sequence *)

type 'a t
(*@ mutable model view : 'a Sequence.t *)


val push : 'a t -> 'a -> unit
(*@ push q x
    ensures q = cons x (old q)
    consumes q as 'a t
    produces q as 'a t
*)

val pop : 'a t -> 'a
(*@ x = pop q
    requires q <> empty
    ensures old q = q ++ (singleton x)
    modifies q
 *)

val length : 'a t -> int
(*@ n = length q
    ensures n = length q
 *)


