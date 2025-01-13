(*@ function logand (x : integer) (y : integer) : integer *)

type t = private { size : int; mutable mask : int }
(*@ with self
    invariant 0 <= self.size <= 63
    invariant 0 <= self.mask < pow 2 self.size *)

(*@ predicate mem (i: integer) (bv: t) = logand bv.mask (pow 2 i) <> 0 *)

val create : int -> t
(*@ checks 0 <= n <= 63
    let bv = create n in
    ensures bv.size = n
    ensures forall i. 0 <= i < n -> not (mem i bv) *)

val add : int -> t -> unit
(*@ checks 0 <= i < bv.size
    modifies bv
    let _ = add i bv in
    ensures forall j. 0 <= j < bv.size ->
              mem j bv <-> i = j \/ mem j (old bv) *)

val mem : int -> t -> bool
(*@ checks 0 <= i < bv.size
    let b = mem i bv in
    ensures b <-> mem i bv *)
