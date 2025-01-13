(**************************************************************************)
(*                                                                        *)
(*  VOCaL -- A Verified OCaml Library                                     *)
(*                                                                        *)
(*  Copyright (c) 2020 The VOCaL Project                                  *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(*@ open Sequence *)

type 'a buffer
(*@ mutable model sequence: 'a sequence
            model capacity: integer
    with self
    invariant length self.sequence <= self.capacity <= Sys.max_array_length *)

val create : int -> 'a -> 'a buffer
(*@ requires 0 < n <= Sys.max_array_length
    let b = create n dummy in
      ensures  b.capacity = n
      ensures  b.sequence = empty *)

val length : 'a buffer -> int
(*@ let n = length b in
      ensures n = length b.sequence *)

val clear : 'a buffer -> unit
(*@ modifies b
    let () = clear b in
      ensures  b.sequence = empty *)

val push : 'a buffer -> 'a -> unit
(*@ requires length b.sequence < b.capacity
    modifies b
    let () = push b elt in
      ensures  length b.sequence = length (old b.sequence) + 1
      ensures  b.sequence = old b.sequence ++ (cons elt empty) *)

val peek : 'a buffer -> 'a
(*@ requires length b.sequence > 0
    let r = peek b in
      ensures  r = b.sequence[0] *)

val pop : 'a buffer -> 'a
(*@ requires length b.sequence > 0
    modifies b
    let r = pop b in
      ensures  length b.sequence = length (old b.sequence) - 1
      ensures  r = (old b.sequence)[0]
      ensures  old b.sequence = cons r b.sequence *)

val get : 'a buffer -> int -> 'a
(*@ requires 0 <= i < length b.sequence
    let r = get b i in
      ensures  r = b.sequence[i] *)

val copy : 'a buffer -> 'a buffer
(*@ let r = copy b in
      ensures length b.sequence = length r.sequence
      ensures b.capacity = r.capacity
      ensures forall i. 0 <= i < length r.sequence ->
        b.sequence[i] = r.sequence[i] *)
