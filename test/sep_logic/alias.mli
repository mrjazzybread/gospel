type t
(*@ mutable model : int *)

val st_eq : t -> t -> bool
(*@ b = st_eq x y
    preserves x as t
    preserves y as t
    ensures b <-> x = y *)

val ph_eq : t -> t -> bool
(*@ b = ph_eq x y
    preserves x as loc
    preserves y as loc
    ensures b <-> x = y *)
