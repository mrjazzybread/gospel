(**************************************************************************)
(*                                                                        *)
(*  VOCaL -- A Verified OCaml Library                                     *)
(*                                                                        *)
(*  Copyright (c) 2020 The VOCaL Project                                  *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(*@ predicate k_values (k: int) (a: int array) =
      forall i. 0 <= i < Sequence.length a -> 0 <= a[i] < k *)

(*@ predicate sorted (a: int array) =
      forall i j. 0 <= i <= j < Sequence.length a -> a[i] <= a[j] *)

val counting_sort : int -> int array -> int array -> unit
(*@ requires 0 < k
    requires k_values k a
    requires Sequence.length a = Sequence.length b
    modifies b
    let () = counting_sort k a b in
      ensures  sorted b
      ensures  Sequence.permut a b *)

val in_place_counting_sort : int -> int array -> unit
(*@ requires 0 < k
    requires k_values k a
    modifies a
    let () = in_place_counting_sort k a in
      ensures  sorted a
      ensures  Sequence.permut (old a) a *)
