val log2 : int -> int
(*@ requires i >= 0
    requires x = pow 2 i
    let r = log2 [i: integer] x in
    ensures r = i *)

val log2b : int -> int
(*@ requires i >= 0
    requires x = pow 2 i
    let r = log2b [i:integer] x in
    ensures r = i *)

val log2_exists : int -> int
(*@ requires exists i. i >= 0 /\ x = pow 2 i
    let r = log2_exists x in
    ensures forall i. x = pow 2 i -> r = i *)

val log2_existsb : int -> int
(*@ requires exists i. i >= 0 /\ x = pow 2 i
    let r = log2_existsb x in
    ensures x = pow 2 r *)
