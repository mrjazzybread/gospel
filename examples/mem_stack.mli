(* Basic operations on ephemeral stacks. *)
type 'a estack
(*@ mutable model : 'a Sequence.t *)

val ecreate : 'a -> 'a estack
(*@ r = ecreate x
      ensures r = Sequence.empty *)

val epush : 'a estack -> 'a -> unit
(*@ epush s x
      modifies s
      ensures  s = Sequence.cons x (old s) *)

val epop : 'a estack -> 'a
(*@ r = epop s
      modifies s
      requires s <> Sequence.empty
      ensures old s = Sequence.cons r s *)

(* Basic operations on persistent stacks. *)
type 'a pstack
(*@ model : 'a Sequence.t *)

val pcreate : 'a -> 'a pstack
(*@ r = pcreate x
      ensures r = Sequence.empty *)

val ppush : 'a pstack -> 'a -> 'a pstack
(*@ r = ppush s x
      modifies s
      ensures  r = Sequence.cons x s *)

val ppop : 'a pstack -> 'a pstack * 'a
(*@ (rs, res) = ppop s
      modifies s
      requires s <> Sequence.empty
      ensures  s = Sequence.cons res rs *)

(* Conversions. *)
val pstack_to_estack : 'a pstack -> 'a estack
(*@ re = pstack_to_estack ps
      ensures re = ps *)

val estack_to_pstack : 'a estack -> 'a pstack
(*@ rp = estack_to_pstack es
      consumes es
      ensures  rp = es *)
