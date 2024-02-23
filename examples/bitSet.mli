(*@ open Set *)


(*@ predicate valid (n : integer) = 
    n >= 0  && n < Sys.word_size
 *)

type t
(*@ model : integer set
    invariant forall x. mem x this -> valid x *)

val singleton : int -> t
(*@ s = singleton v
    requires valid v
    ensures s = singleton v
 *)

val add : int -> t -> t
(*@ s2 = add i s1
    require valid i
    ensures s2 = add i s1
*)

val remove : int -> t -> t
(*@ s2 = add i s1
    requires valid i
    ensures s2 = remove i s1
 *)

val is_singleton : t -> bool
(*@ b = is_singleton s
    ensures b <-> (exists v. s = singleton s)
*)