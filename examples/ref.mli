type 'a ref
(*@ mutable model : 'a *)

val ref : 'a -> 'a ref
(*@ r = ref v
    ensures r = v *)

val get : 'a ref -> 'a
(*@ v = get r
    ensures r = v
*)

val update : 'a ref -> 'a -> unit
(*@ update r v
    modifies r @ 'a ref
    ensures r = v *)
