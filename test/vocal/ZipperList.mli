(**************************************************************************)
(*                                                                        *)
(*  VOCaL -- A Verified OCaml Library                                     *)
(*                                                                        *)
(*  Copyright (c) 2020 The VOCaL Project                                  *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(** Zippers for lists *)

type 'a t
(*@ model seq: 'a sequence
    model idx: integer
    with self
    invariant 0 <= self.idx <= Sequence.length self.seq *)

val empty : unit -> 'a t
(*@ let z = empty () in
    ensures z.seq = Sequence.empty
    ensures z.idx = 0 *)
(* could be deduced from invariant *)

val is_empty : 'a t -> bool
(*@ let b = is_empty z in
    ensures b <-> z.seq = Sequence.empty *)

val length : 'a t -> int
(*@ let r = length z in
    ensures r = Sequence.length z.seq *)

val to_list : 'a t -> 'a list
(*@ let l = to_list z in
    ensures z.seq = l *)

val make : 'a list -> 'a t
(*@ let z = make l in
    ensures z.seq = l
    ensures z.idx = 0 *)

val move_left : 'a t -> 'a t
(*@ requires 0 < z.idx
    let r = move_left z in
    ensures  r.seq = z.seq
    ensures  r.idx = z.idx - 1 *)

val insert_left : 'a -> 'a t -> 'a t
(*@ let r = insert_left x z in
    ensures  r.seq = Sequence.snoc z.seq[.. z.idx] x ++ z.seq[z.idx ..]
    ensures  r.idx = z.idx + 1 *)

val remove_left : 'a t -> 'a t
(*@ requires 0 < z.idx
    let r = remove_left z in
    ensures  r.seq = z.seq[.. z.idx - 1] ++ z.seq[z.idx ..]
    ensures  r.idx = z.idx - 1*)

val move_all_left : 'a t -> 'a t
(*@ let r = move_all_left z in
      ensures r.seq = z.seq
      ensures r.idx = 0 *)

val move_right : 'a t -> 'a t
(*@ requires z.idx < Sequence.length z.seq
    let r = move_right z in
    ensures  r.seq = z.seq
    ensures  r.idx = z.idx + 1 *)

val insert_right : 'a -> 'a t -> 'a t
(*@ let r = insert_right x z in
    ensures  r.seq = z.seq[.. z.idx] ++ Sequence.cons x z.seq[z.idx ..]
    ensures  r.idx = z.idx *)

val remove_right : 'a t -> 'a t
(*@ requires z.idx < Sequence.length z.seq
    let r = remove_right z in
    ensures  r.seq = z.seq[.. z.idx] ++ z.seq[z.idx + 1 ..]
    ensures  r.idx = z.idx *)

val move_all_right : 'a t -> 'a t
(*@ let r = move_all_right z in
    ensures r.seq = z.seq
    ensures r.idx = Sequence.length z.seq *)

val is_focused : 'a t -> bool
(*@ let b = is_focused z in
    ensures b <-> z.idx < Sequence.length z.seq *)

val focused : 'a t -> 'a option
(*@ let r = focused z in
    ensures match r with
            | None   -> z.idx = Sequence.length z.seq
            | Some x -> z.idx < Sequence.length z.seq /\ x = z.seq[z.idx] *)
