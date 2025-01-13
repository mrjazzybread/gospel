module type S = sig
  type 'a ref
  (*@ mutable model : 'a *)

  val ref : 'a -> 'a ref
  (*@ let r = ref v in
        produces r @ 'a ref
        ensures r = v *)

  val get : 'a ref -> 'a
  (*@ preserves r @ 'a ref
      let v = get r in
        ensures r = v *)

  val update : 'a ref -> 'a -> unit
  (*@ modifies r @ 'a ref
      let _ = update r v in
        ensures r = v *)
end
