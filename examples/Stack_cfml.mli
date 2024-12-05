module type S = sig
  type 'a t
  (*@ mutable model : 'a Sequence.t *)

  val create : unit -> 'a t
  (*@ let q = create () in
        ensures q = Sequence.empty *)

  val is_empty : 'a t -> bool
  (*@ preserves q @ 'a t
      let b = is_empty q in
        ensures b <-> q = Sequence.empty *)

  val push : 'a t -> 'a -> unit
  (*@ modifies p @ 'a t
      let _ = push p x in
        ensures p = Sequence.cons x (old p) *)

  val pop : 'a t -> 'a
  (*@ modifies p @ 'a t
      requires p <> Sequence.empty
      let r = pop p in
      ensures (old p) = Sequence.cons r p *)

  val clear : 'a t -> unit
  (*@ modifies p @ 'a t
      let _ = clear p in
        ensures p = Sequence.empty *)

  val concat : 'a t -> 'a t -> unit
  (*@ modifies q1 @ 'a t
      modifies q2 @ 'a t
      let _ = concat q1 q2 in
        ensures q1 = old (q1 ++ q2)
        ensures q2 = Sequence.empty *)

  val rev_append : 'a t -> 'a t -> unit
  (*@ modifies p1 @ 'a t
      modifies p2 @ 'a t
      let _ = rev_append p1 p2 in
        ensures p1 = Sequence.empty
        ensures p2 = old (Sequence.rev p1 ++ p2) *)
end
