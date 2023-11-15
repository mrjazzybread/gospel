(*@ open Sequence *)

type  t
(*@ mutable model : int Sequence.t *)

(* 
   Type 'a t
   
   Type 'a @model_t = {view : 'a Sequence.t}

   Predicate @R_t : ('a -> '@model_a -> Prop) -> 'a t -> 'a @model_t -> Prop 
 *)

val push : t -> int -> unit
(*@ push q x
    ensures q = cons x (old q)
    modifies q
    consumes x
*)

val pop : t -> int
(*@ x = pop q
    requires q <> empty
    ensures old q = q ++ (singleton x)
    modifies q
 *)

val length : t -> int
(*@ n = length q
    ensures n = length q
 *)


(* predicate is_eq (A : Type) 
(* this predicate does not hold for unwoned mutable structures and functions*)

val st_eq : 'a -> 'a -> bool
(* b = st_eq x y
    requires is_eq 'a
    ensures b <-> x = y
 *)

(* predicate unowned (A : Type) *)



(*
{ R x Mx * R y My * [unowned R] } ph_eq x y {[x = y]}

val ph_eq : 'a -> 'a -> bool
(* b = st_eq x y
    requires addressable 'a
    ensures b <-> &x = &y
 *)

{ [is_loc x && is_loc y] } ph_eq x y {[x = y]}

 predicates over types, type classes *)
(* increase clarity in spatial specs
   adressable vs unowned *)
 *)
