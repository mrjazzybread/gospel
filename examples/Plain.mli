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

(**This is a priority queue whose keys are {i low} nonnegative integers. *)

(**This queue is optimized for throughput, that is, speed. When an element is
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
(*@ mutable model : ('a * integer) bag *)

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

(*@ predicate is_min (x : 'a) (i : integer) (q : ('a * integer) bag) =
      (x, i) ∈ q ∧ ∀ j y. j < i -> (y, j) ∉ q *)

val extract : 'a t -> 'a option
(**[extract q] extracts an element out of the queue [q] and returns it. This
   element has minimum priority among all of the elements that are currently
   present in the queue. If the queue is empty, [None] is returned.

   Time complexity: {m O(p)}. If the queue is used in a {i monotonic} manner
   (that is, if the priority that is used in every call to {!add} is at least as
   high as the priority of the last element that was returned by {!extract})
   then the time complexity of {m n} calls to {!extract} is only {m O(n+p)}.
   Indeed, in this monotonic scenario, the cost of scanning the queue's main
   array, so as to find the next element with minimum priority, is shared
   between all invocations of {!extract}. *)
(*@ x = extract q
    modifies q
    ensures q = ∅ -> x = None
    ensures
      q ≠ ∅ ->
        ∃ x'. Some x' = x ∧
        ∃ i. is_min x' i (old q) ∧ q ∪ {: (x', i) :} = old q *)

(*val extract' : 'a t -> ('a * priority) option
(**[extract' q] extracts an element out of the queue [q] and returns a pair of
   this element and its priority. This element has minimum priority among all of
   the elements that are currently present in the queue. If the queue is empty,
   [None] is returned.

   Time complexity: see {!extract}. *)
 TODO x = extract' q
    modifies q
    ensures q = ∅ -> x = None
    ensures
      q ≠ ∅ ->
        ∃ x' i. Some (x', i) = x ∧ is_min x' i q ∧ q ∪ {: (x', i) :} = old q *)

val is_empty : 'a t -> bool
(**[is_empty q] tests whether the queue [q] is empty.

   Time complexity: {m O(1)}. *)
(*@ b = is_empty q
    ensures b ↔ q = ∅ *)

val cardinal : 'a t -> int
(**[cardinal q] returns the number of elements in the queue [q].

   Time complexity: {m O(1)}. *)
(*@ n = cardinal q
    ensures n = cardinal q *)

val reset : 'a t -> unit
(**[reset q] empties the queue [q], freeing up the space that the queue occupies
   in memory. The queue [q] becomes identical to a queue that has just been
   created by {!create}.

   Time complexity: {m O(1)}. *)
(*@ () = reset q
    modifies q
    ensures q = ∅ *)

(* val iter : 'a t -> ('a -> unit) -> unit *)
(* (\**[iter q yield] enumerates the elements of the queue [q], by increasing order *)
(*    of priority, by passing them to the function [yield]. This function must not *)
(*    modify the queue [q]. *\) *)
(* (\* iter q f *)
(*    ? *\) *)

(* val repeat : 'a t -> ('a -> unit) -> unit *)
(* (\**[repeat q yield] repeatedly extracts an element with minimum priority out of *)
(*    [q] and passes it to [yield] (which may insert new elements into [q]), until *)
(*    [q] is exhausted. *)

(*    Time complexity: the total cost of {m n} calls to {!extract} and {m n} *)
(*    invocations of the function [yield]. *\) *)
(* (\* repeat q f *)
(*    modifies q *)
(*    spec *)
(*    (\* f [I] [visited] [remaining] x *)
(*       modifies q *)
(*       requires {:x:} ∪ visited ∪ remaining = q *)
(*       requires I visited *)
(*       ensures I (add visited x) *\) *)
(*    ensures q = ∅ *\) *)

(* (\**/**\) *)

(* val check : 'a t -> unit *)
(* (\**[check] is used only during testing. *\) *)
