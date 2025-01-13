module type S = sig
  type 'a t
  (*@ mutable model view : 'a sequence *)

  val create : unit -> 'a t
  (*@ let s = create () in
        ensures s.view = Sequence.empty *)

  val size : 'a t -> int
  (*@ let n = size s in
        ensures n = Sequence.length s.view *)

  val is_empty : 'a t -> bool
  (*@ let b = is_empty s in
        ensures b <-> s.view = Sequence.empty *)

  val push : 'a -> 'a t -> unit
  (*@ modifies s
      let _ = push x s in
        ensures  s.view = Sequence.cons x (old s.view) *)

  val pop : 'a t -> 'a
  (*@ requires s.view <> Sequence.empty
      modifies s
      let x = pop s in
        ensures  s.view = (old s.view)[1 ..] *)

  val clear : 'a t -> unit
  (*@ modifies s
      let _ = clear s in
        ensures  s.view = Sequence.empty *)

  val concat : 'a t -> 'a t -> unit
  (*@ modifies s1, s2
      let _ = concat s1 s2 in
        ensures s1.view = (old s1.view ++ old s2.view)
        ensures s2.view = Sequence.empty *)
end
