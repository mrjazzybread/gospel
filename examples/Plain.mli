(******************************************************************************)
(*                                                                            *)
(*                                  IntPQueue                                 *)
(*                                                                            *)
(*                       François Pottier, Inria Paris                        *)
(*                                                                            *)
(*       Copyright 2025--2025 Inria. All rights reserved. This file is        *)
(*       distributed under the terms of the GNU Library General Public        *)
(*       License, with an exception, as described in the file LICENSE.        *)
(*                                                                            *)
(******************************************************************************)

(** This is a priority queue whose keys are {i low} nonnegative integers. *)

(** This queue is optimized for throughput, that is, speed. When an element is
    extracted out of the queue, the queue may retain a pointer to this element.
    This creates a memory leak: that is, the existence of this pointer can
    prevent the garbage collector from freeing this element. In practice, we do
    not expect this to be a problem, especially in scenarios where the queue
    itself is not long-lived. *)

(*@ open Bag *)

type priority = int
(**A priority is a nonnegative integer. *)

type 'a t
(**A priority queue is an abstract mutable data structure. It can be thought of
   as a bag of elements, where each element carries a certain priority. *)
(*@ mutable model : (val * integer) bag *)

val create : unit -> 'a t
(**[create()] creates an empty priority queue.

   Time complexity: {m O(1)}. *)
(*@ q = create ()
    ensures q = ∅ *)

val add : 'a t -> 'a -> priority -> unit
(**[add q x i] inserts the element [x] with priority [i] into the queue [q].

   Time complexity: {m O(1)} (amortized). *)
(*@ add q x i
    modifies q
    ensures q = add (x, i) (old q) *)
