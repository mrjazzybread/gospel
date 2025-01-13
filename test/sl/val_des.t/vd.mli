val add :  int -> int -> int
(*@ let z = add x y in
    ensures z = x + y *)

type elt

val st_eq : elt -> elt -> bool
(*@ let b = st_eq x y in
    ensures b <-> x = y *)

val identity : elt -> elt

type 'a pointer
(*@ mutable model : 'a *)

val get : 'a pointer -> 'a
(*@ let r = get p in
    ensures r = p *)

val set : 'a pointer -> 'a -> unit
(*@ modifies p @ 'a pointer
    let () = set p x in
    ensures p = x *)

val ph_eq : 'a pointer -> 'a pointer -> bool
(*@ preserves x @ loc
    preserves y @ loc
    let b = ph_eq x y in
    ensures b <-> x = y *)

type 'a array
(*@ mutable model elts : 'a sequence
    model size : int *)

val set_i : 'a array -> int -> 'a -> unit
(*@ modifies arr @ 'a array
    let () = set_i arr i x in
    ensures arr.elts = Sequence.set (old arr.elts) i x *)

val concat : 'a array -> 'a array -> 'a array
(*@ let r = concat arr1 arr2 in
    ensures r.elts = arr1.elts ++ arr2.elts *)
