(**************************************************************************)
(*                                                                        *)
(*  VOCaL -- A Verified OCaml Library                                     *)
(*                                                                        *)
(*  Copyright (c) 2020 The VOCaL Project                                  *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

module Make (X : sig
  (* FIXME: use ComparableType.S instead *)
  type t

  (*@ function cmp: t -> t -> int *)

  (*@ axiom is_pre_order: Order.is_pre_order cmp *)

  val compare : t -> t -> int
  (*@ let r = compare x y in
        ensures r = cmp x y *)
end) : sig
  type elt = X.t
  type t
  (*@ model bag : elt bag *)

  val empty : unit -> t
  (*@ let h = empty () in
        ensures Bag.cardinal h.bag = 0
        ensures forall x. Bag.multiplicity x h.bag = 0 *)

  val is_empty : t -> bool
  (*@ let b = is_empty h in
        ensures b <-> Bag.empty = h.bag *)

  val merge : t -> t -> t
  (*@ let h = merge h1 h2 in
        ensures Bag.cardinal h.bag = Bag.cardinal h1.bag + Bag.cardinal h2.bag
        ensures forall x. Bag.multiplicity x h.bag = Bag.multiplicity x h1.bag + Bag.multiplicity x h2.bag *)

  val insert : elt -> t -> t
  (*@ let h' = insert x h in
        ensures Bag.multiplicity x h'.bag = Bag.multiplicity x h.bag + 1
        ensures forall y. y <> x -> Bag.multiplicity y h'.bag = Bag.multiplicity y h.bag
        ensures Bag.cardinal h'.bag = Bag.cardinal h.bag + 1 *)

  (*@ predicate mem        (x: elt) (h: t) = Bag.multiplicity x h.bag > 0 *)
  (*@ predicate is_minimum (x: elt) (h: t) =
        mem x h /\ forall e. mem e h -> X.cmp x e <= 0 *)

  (*@ function minimum (h: t) : elt *)
  (*@ axiom min_def: forall h. 0 < Bag.cardinal h.bag -> is_minimum (minimum h) h *)

  val find_min : t -> elt
  (*@ requires Bag.cardinal h.bag > 0
      let x = find_min h in
        ensures  x = minimum h *)

  val delete_min : t -> t
  (*@ requires Bag.cardinal h.bag > 0
      let h' = delete_min h in
        ensures  let x = minimum h in Bag.multiplicity x h'.bag = Bag.multiplicity x h.bag - 1
        ensures  forall y. y <> minimum h -> Bag.multiplicity y h'.bag = Bag.multiplicity y h.bag
        ensures  Bag.cardinal h'.bag = Bag.cardinal h.bag - 1 *)
end
