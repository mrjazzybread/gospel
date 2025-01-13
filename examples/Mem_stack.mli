(*@ type 'a memory *)

(*@ predicate extend (m m': 'a memory) *)
(*@ axiom refl: forall m: 'a memory. extend m m *)
(*@ axiom tran: forall m1 m2 m3: 'a memory.
      extend m1 m2 -> extend m2 m3 ->
      extend m1 m3 *)

(*@ val create_mem : unit -> 'a memory *)
module Ephemeral : sig
  type 'a stack
  (*@ mutable model : 'a memory -> 'a Sequence.t *)

  (*@ axiom stack_mon:
        forall m m': 'a memory, e: 'a stack.
        extend m m' ->
        e m = e m' *)

  val ecreate : 'a -> 'a stack
  (*@ let r = ecreate [m: 'a memory] x in
        produces r @ 'a stack
        ensures r m = Sequence.empty *)

  val epush : 'a stack -> 'a -> unit
  (*@ modifies s @ 'a stack
      let _ = epush [m: 'a memory] s x in
        ensures  s m = Sequence.cons x (old s m) *)

  val epop : 'a stack -> 'a
  (*@ modifies s @ 'a stack      
      requires s m <> Sequence.empty
      let r = epop [m: 'a memory] s in
        ensures  (old s m) = Sequence.cons r (s m) *)
end

module Persistent : sig
  type 'a stack
  (*@ model pview: 'a memory -> 'a Sequence.t
      mutable model internal: unit *)

  (*@ axiom pstack_mon:
        forall m m': 'a memory, p: 'a stack.
        extend m m' ->
        p.pview m = p.pview m' *)

  val pcreate : 'a -> 'a stack
  (*@ let r, [m': 'a memory] = pcreate [m: 'a memory] x in
        ensures r.pview m' = Sequence.empty
        ensures extend m m' *)

  val ppush : 'a stack -> 'a -> 'a stack
  (*@ modifies s
      let r, [m': 'a memory] = ppush [m: 'a memory] s x in
        ensures  r.pview m' = Sequence.cons x (s.pview m)
        ensures  extend m m' *)

  val ppop : 'a stack -> 'a stack * 'a
  (*@ modifies s
      requires s.pview m <> Sequence.empty
      let (rs, res) = ppop [m: 'a memory] s in
        ensures  s.pview m = Sequence.cons res (rs.pview m) *)
end

val pstack_to_estack : 'a Persistent.stack -> 'a Ephemeral.stack
(*@ let re = pstack_to_estack [m: 'a memory] ps in
      ensures re m = ps.Persistent.pview m *)

val estack_to_pstack : 'a Ephemeral.stack -> 'a Persistent.stack
(*@ consumes es
    let rp, [m': 'a memory] = estack_to_pstack [m: 'a memory] es in
      ensures  rp.Persistent.pview m' = (old es) m
      ensures  extend m m' *)
