module type S = sig
  type 'a t
  (*@ mutable model : 'a sequence *)

  val create : unit -> 'a t
  (*@ let a = create () in
        produces a @ 'a t
        ensures a = Sequence.empty *)

  val make : int -> 'a -> 'a t
  (*@ let a = make n e in
        produces a @ 'a t
        ensures a = Sequence.init n (fun _ -> e) *)

  val get : 'a t -> int -> 'a
  (*@ requires 0 <= i < Sequence.length a
      preserves a @ 'a t
      let r = get a i in
        ensures r = a[i] *)

  val set : 'a t -> int -> 'a -> unit
  (*@ requires 0 <= i < Sequence.length a
      modifies a @ 'a t
      let _ = set a i e in
        ensures a = Sequence.set (old a) i e *)

  val length : 'a t -> int
  (*@ preserves a @ 'a t
      let l = length a in
        ensures l = Sequence.length a *)

  val is_empty : 'a t -> bool
  (*@ preserves a @ 'a t
      let b = is_empty a in
        ensures b <-> (a = Sequence.empty) *)

  val find_last : 'a t -> 'a option
  (*@ preserves a @ 'a t
      let r = find_last a in
        ensures match r with
        |None -> a = Sequence.empty
        |Some r -> r = a[Sequence.length a - 1] *)

  val copy : 'a t -> 'a t
  (*@ preserves a @ 'a t
      let c = copy a in
        ensures c = a *)

  val add_last : 'a t -> 'a -> unit
  (*@ modifies a @ 'a t
      let _ = add_last a e in
        ensures  a = (old a) ++ (Sequence.singleton e) *)

  val append : 'a t -> 'a t -> unit
  (*@ modifies a1 @ 'a t
      let _ = append a1 a2 in
        ensures a1 = (old a1) ++ a2 *)

  val pop_last_opt : 'a t -> 'a option
  (*@ modifies a @  'a t
      let r = pop_last_opt a in
        ensures match r with
        |None -> a = Sequence.empty && old a = Sequence.empty
        |Some r -> a ++ Sequence.singleton r = old a *)

  val remove_last : 'a t -> unit
  (*@ modifies a @ 'a t
      let _ = remove_last a in
        ensures old a = Sequence.empty -> a = Sequence.empty
        ensures a <> Sequence.empty ->
                a = old (a[.. Sequence.length a - 1]) *)

  val truncate : 'a t -> int -> unit
  (*@ modifies a @ 'a t
      requires n >= 0
      let _ = truncate a n in
        ensures n >= Sequence.length a -> a = old a
        ensures n < Sequence.length a -> a = old (a[.. n]) *)

  val clear : 'a t -> unit
  (*@ modifies a @ 'a t
      let _ = clear a in
        ensures a = Sequence.empty *)
end
