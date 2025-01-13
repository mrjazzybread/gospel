val x : int ref
val incr_x : unit -> unit
(*@ modifies x
    let () = incr_x () in
    ensures x.contents = old x.contents + 1 *)

type t

val y : t
(*@ ensures y = y *)

val modify_y : unit -> unit
(*@ modifies y *)
