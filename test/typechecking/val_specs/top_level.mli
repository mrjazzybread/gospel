val x : int ref
val incr_x : unit -> unit
(*@ incr_x ()
    modifies x
    ensures x = old x + 1 *)

type t
(*@ mutable model : integer *)

val y : t
(*@ ensures y >= 0 *)

val modify_y : unit -> unit
(*@ modify_y ()
    modifies y *)
