(*@ open Sequence *)

type 'a t
(*@ mutable model view : 'a Sequence.t *)

 
val push : 'a t -> 'a -> unit
(*@ push q n
    ensures q.view = cons n (old q.view)
    modifies q  *)

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
    ensures n = length q
    
 *)



(* 
ghost val to_loc 'a t -> unit
 (* @ to_loc q
      consumes q @ 'a t
      produces q @ loc t *)

val peek : t -> int array 
  (*@ n = peek q
    ensures last q v
    consumes q @ t
    produces q @ t

 *) *)


(*
  type 'a cell = Nil | Cons of {mutable c : 'a t}  and
  type 'a t = {e : 'a; next : 'a cell}

  type 'a t = {mutable first : 'a list; mutable last : 'a list}
 *)

