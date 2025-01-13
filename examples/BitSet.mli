module type S = sig
  (*@ open Set *)

  (*@ predicate valid (n : integer) =
    n >= 0  && n < Sys.word_size *)

  type t
  (*@ model : int set *)

  val singleton : int -> t
  (*@ requires valid v
      let s = singleton v in
        ensures s = singleton v *)

  val add : int -> t -> t
  (*@ requires valid i
      let s2 = add i s1 in
        ensures s2 = add i s1 *)

  val remove : int -> t -> t
  (*@ requires valid i
      let s2 = remove i s1 in
        ensures s2 = remove i s1 *)

  val is_singleton : t -> bool
  (*@ let b = is_singleton s in
        ensures b <-> (exists v. s = Set.singleton v) *)
end
