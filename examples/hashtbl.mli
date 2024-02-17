(*@ open Sequence *)
(*@ open Fmap *)

type t
(*@ mutable model : (int, int sequence) fmap  *)

val create : int -> t
(*@ tbl = create n
    ensures tbl = empty *)

val clear : t -> t
(*@ clear tbl
    ensures tbl = empty *)

val copy : t -> t
(*@ c = copy tbl
    ensures forall k. tbl[k] = c[k] *)

val add : t -> int -> int -> unit
(*@ add tbl k v
    modifies tbl
    ensures tbl = old(tbl[k -> cons v (tbl[k])]) 
*)

val find_opt : t -> int -> int option
(*@ r = find_opt tbl k
    preserves tbl
    ensures match r with 
    |None -> tbl[k] = Sequence.empty
    |Some x -> hd (tbl[k]) = x
 *)

val find_all : t -> int -> int list
(*@ l = find_all tbl k
    ensures l = tbl[k] *)

val mem :  t -> int -> bool
(*@ b = mem tbl k
    ensures b <-> tbl[k] <> Sequence.empty 
 *)

val remove : t -> int -> unit
(*@ remove tbl k
    modifies tbl
    ensures tbl[k] = Sequence.empty -> tbl = old tbl
    ensures tbl[k] <> Sequence.empty -> tbl = old (tbl[k -> tl (tbl[k])])
 *)

val replace : t -> int -> int -> unit
(*@ replace tbl k v
    modifies tbl 
    ensures tbl[k] = Sequence.empty -> tbl = tbl[k -> (singleton v)]
    ensures tbl[k] <> Sequence.empty -> 
       let tail = old (tl (tbl[k])) in 
       tbl = old (tbl[k -> cons v tail])
 *)
