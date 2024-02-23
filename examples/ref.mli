type ref
(*@ mutable model : int *)

val ref : int -> ref
(*@ r = ref v
    ensures r = v *)

val (!) : ref -> int
(*@ v = (!) r 
    ensures r = v
*) 

val (:=) : ref -> int -> unit
(*@ (:=) r v
    modifies r @ ref
    ensures r = v *)
