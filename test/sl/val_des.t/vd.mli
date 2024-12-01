val add :  int -> int -> int
(*@ z = add x y
    ensures z = x + y *)

type elt

val st_eq : elt -> elt -> bool
(*@ b = st_eq x y
    ensures b <-> x = y *)

val identity : elt -> elt

type 'a pointer
(*@ mutable model : 'a *)

val get : 'a pointer -> 'a
(*@ r = get p
    ensures r = p *)

val set : 'a pointer -> 'a -> unit
(*@ set p x
    modifies p @ 'a pointer
    ensures p = x *)

val ph_eq : 'a pointer -> 'a pointer -> bool
(*@ b = ph_eq x y
    preserves x @ loc
    preserves y @ loc
    ensures b <-> x = y *)

type 'a array
(*@ mutable model elts : 'a sequence
    model size : int *)

val set_i : 'a array -> int -> 'a -> unit
(*@ set_i arr i x
    modifies arr @ 'a array
    ensures arr.elts = Sequence.set (old arr.elts) i x *)

val concat : 'a array -> 'a array -> 'a array
(*@ r = concat arr1 arr2
    ensures r.elts = arr1.elts ++ arr2.elts *)
