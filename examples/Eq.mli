module type S = sig
  type t
  (*@ ephemeral *)

  val st_eq : t -> t -> bool
  (*@ preserves x @ t
      preserves y @ t
      let b = st_eq x y in
        ensures b <-> x = y *)

  val ph_eq : t -> t -> bool
  (*@ preserves x @ loc
      preserves y @ loc
      let b = ph_eq x y in
        ensures b <-> x = y *)
end
