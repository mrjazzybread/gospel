(**************************************************************************)
(*                                                                        *)
(*  Gospel -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(** This file contains the Gospel standard library. *)

(** The following are not defined in the Gospelstdlib but are built-in in
    Gospel:

    - [type unit]
    - [type string]
    - [type char]
    - [type float]
    - [type bool]
    - [type integer]
    - [type int]

    - [type 'a option]
    - [function None: 'a option]
    - [function Some (x: 'a) : 'a option]
    - [type 'a list]
    - [function ([]): 'a list]
    - [function (::) (x: 'a) (l: 'a list) : 'a list]

    - [predicate (=) (x y: 'a)] *)

(** The rest of this module is the actual content of the Gospel module
    [Gospelstdlib]. This module is automatically opened in Gospel
    specifications. *)

(*@ type 'a sequence *)
(** The type for finite sequences. *)

(*@ type 'a bag *)
(** The type for multisets. *)

(*@ type 'a set *)
(** The type for sets. *)

(*@ type ('a, 'b) map = 'a -> 'b*)
(** The type for total maps *)

(** {1 Arithmetic}

    The type [integer] is built-in. This is the type of arbitrary precision
    integers, not to be confused with OCaml's type [int] (machine, bounded
    integers). *)

(*@ function succ (x: integer) : integer *)
(*@ function pred (x: integer) : integer *)

(*@ function (-_) (x: integer) : integer *)
(*@ function (+) (x y: integer) : integer *)
(*@ function (-) (x y: integer) : integer *)
(*@ function ( * ) (x y: integer) : integer *)
(*@ function (/) (x y: integer) : integer *)
(*@ function mod (x y: integer) : integer *)

(*@ function pow (x y: integer) : integer *)
(*@ function abs (x:integer) : integer *)

(*@ function min (x y : integer) : integer *)
(*@ function max (x y : integer) : integer *)

(** {2 Comparisons} *)

(*@ predicate (>) (x y: integer) *)
(*@ predicate (>=) (x y: integer) *)
(*@ predicate (<) (x y: integer) *)
(*@ predicate (<=) (x y: integer) *)

(*@ function max_int : integer *)
(*@ function min_int : integer *)
(** {1 Sequences} *)

(*@ function (++) (s s': 'a sequence) : 'a sequence *)
(** [s ++ s'] is the sequence [s] followed by the sequence [s']. *)

(*@ function ([_]) (s: 'a sequence) (i: integer): 'a *)

(** [s[i]] is the [i]th element of the sequence [s]. *)

(*@ function ([->]) (f: 'a -> 'b) (x:'a) (y: 'b) : 'a -> 'b =
      fun arg -> if arg = x then y else f x *)

(** {1 Bags} *)
module Sequence : sig
  (*@ type 'a t = 'a sequence *)
  (** An alias for {!sequence} *)

  (*@ function length (s: 'a sequence): integer *)
  (*@ function ([_.._]) (s: 'a sequence) (i1: integer) (i2: integer): 'a sequence *)
  (*@ function ([_..]) (s: 'a sequence) (i: integer): 'a sequence = s[i .. length s] *)
  (*@ function ([.._]) (s: 'a sequence) (i: integer): 'a sequence = s[0 .. i] *)

  
  (*@ predicate in_range (s : 'a t) (i : integer) = 0 <= i < length s *)

  (*@ axiom length_nonneg : forall s. 0 <= length s *)
  (*@ axiom append_length : forall s s'. length (s++s') = length s + length s' *)

  (*@ axiom append_elems_left :  forall s s' i. 0 <= i < length s -> (s ++ s')[i] = s[i] *)
  (*@ axiom append_elems_right :
     forall s s' i.
      length s <= i < length s + length s' ->
      (s ++ s')[i] = s'[i - length s] *)

  (*@ axiom subseq : forall s i i1 i2. i1 <= i < i2 -> s[i] = (s[i1 .. i2])[i-i1] *)
  (* axiom subseq_len : TODO *)
  (* axiom subseq_left_to_right : TODO *)

  (*@ function init (n: integer) (f: integer -> 'a) : 'a t *)
  (*@ axiom init_length : forall n f. n >= 0 -> length (init n f) = n *)
  (* TODO : replace init with notation *)
  (*@ axiom init_elems : forall n f.
        forall i. 0 <= i < n -> (init n f)[i] = f i *)
  (** [init n f] is the sequence containing [f 0], [f 1], [...] , [f n]. *)

  (*@ function empty : 'a t *)
  (*@ axiom empty_length : length empty = 0 *)

  (*@ function singleton (x: 'a) : 'a t = init 1 (fun _ -> x) *)
  (** [singleton] is the sequence containing [x]. *)

  (*@ function cons (x: 'a) (s: 'a t): 'a t = (singleton x) ++ s *)
  (** [cons x s] is the sequence containing [x] followed by the elements of [s]. *)

  (*@ function snoc (s: 'a t) (x: 'a): 'a t = s ++ (singleton x) *)
  (** [snoc s x] is the sequence containing the elements of [s] followed by [x]. *)

  (*@ function hd (s: 'a t) : 'a = s[0] *)
  (** When [s] contains one or more elements, [hd s] is the first element of
      [s]. *)

  (*@ function tl (s: 'a t) : 'a t = s[1 ..] *)
  (** When [s] contains one or more elements, [tl s] is the sequence of the
      elements of [s], starting at position 2. *)

  (*@ function append (s1 : 'a t) (s2 : 'a t) : 'a t = s1 ++ s2 *)
  (** [append s s'] is [s ++ s']. *)

  (*@ function multiplicity (x : 'a) (s: 'a t) : integer *)
  (*@ axiom mult_empty : forall x. multiplicity x empty = 0 *)
  (*@ axiom mult_cons : forall s x.
         1 + multiplicity x s = multiplicity x (cons x s) *)
  (*@ axiom mult_cons_neutral : forall s x1 x2.
         x1 <> x2 -> multiplicity x1 s = multiplicity x1 (cons x2 s) *)
  (*@ axiom mult_length : forall x s. 0 <= multiplicity x s <= length s *)

  (*@ predicate mem (x: 'a) (s: 'a t) = multiplicity x s > 0 *)
  (** [mem s x] holds iff [x] is in [s]. *)

  (*@ function map (f: 'a -> 'b) (s: 'a t) : 'b t *)
  (*@ axiom map_elems : forall i f s.
       0 <= i < length s -> (map f s)[i] = f (s[i]) *)
  (** [map f s] is a sequence whose elements are the elements of [s],
      transformed by [f]. *)

  (*@ function filter (f: 'a -> bool) (s: 'a t) : 'a t *)
  (*@ axiom filter_elems :
        forall f s x. mem x s -> f x -> mem x (filter f s) *)
  (** [filter f s] is a sequence whose elements are the elements of [s] that
      satisfy [f]*)

  (*@ function get (s: 'a t) (i: integer) : 'a = s[i] *)
  (** [get s i] is [s[i]]. *)

  (*@ function set (s: 'a t) (i: integer) (x: 'a): 'a t *)
  (*@ axiom set_elem : forall s i x. 0 <= i < length s -> (set s i x)[i] = x *)
  (*@ axiom set_elem_other :
    forall s i1 i2 x. i1 <> i2 ->
    0 <= i1 < length s ->
    0 <= i2 < length s ->
    (set s i1 x)[i2] = s[i2] *)

  (** [set s i x] is the sequence [s] where the [i]th element is [x]. *)

  (*@ function rev (s: 'a t) : 'a t *)
  (*@ axiom rev_length : forall s. length s = length (rev s) *)
  (*@ axiom rev_elems  : forall i s. 0 <= i < length s ->
       (rev s)[i] = s[length s - 1 - i] *)

  (** [rev s] is the sequence containing the same elements as [s], in reverse
      order. *)

  (*@ function fold (f: 'a -> 'b -> 'a) (acc: 'a) (s: 'b sequence) : 'a *)
  (*@ axiom fold_empty : forall f acc. fold f acc empty = acc *)
  (*@ axiom fold_cons : forall f acc x l.
         fold f acc (cons x l) = fold f (f acc x) l *)
  (** [fold f acc s] is [f (... (f (f acc s[0]) s[1]) ...) s[n-1]], where [n] is
      the length of [s]. *)
  (*@ axiom extensionality : forall s1 s2.
        length s1 = length s2 ->
        (forall i. 0 <= i < length s1 -> s1[i] = s2[i]) ->
        s1 = s2
  *)

  (*@ function of_list (s : 'a list) : 'a sequence *)
  (*@ coercion *)
  (* <------ to be removed *)
  (*@ function fold_left (f: 'a -> 'b -> 'a) (acc: 'a) (s: 'b sequence) : 'a *)
  (*@ function fold_right (f: 'b -> 'a -> 'a) (s: 'b sequence) (acc: 'a) : 'a *)
end

(* TODO : List module *)

module Bag : sig
  (*@ type 'a t = 'a bag *)
  (** An alias for ['a bag]. *)

  (*@ function multiplicity (x: 'a) (b: 'a t): integer *)
  (*@ axiom well_formed : forall b x. multiplicity b x >= 0 *)
  (** [multiplicity x b] is the number of occurrences of [x] in [s]. *)

  (*@ function empty : 'a t *)
  (*@ axiom empty_mult : forall x. multiplicity x empty = 0 *)
  (** [empty] is the empty bag. *)

  (*@ function init (f : 'a -> integer) : 'a t *)
  (*@ axiom init_axiom : forall f x. max 0 (f x) = multiplicity x (init f) *)

  (*@ function add (x: 'a) (b: 'a t) : 'a t *)
  (*@ axiom add_mult_x : forall b x. multiplicity x (add x b) = 1 + multiplicity x b *)
  (*@ axiom add_mult_neg_x : forall x y b. x <> y ->
    multiplicity y (add x b) = (multiplicity y b) *)
  (** [add x b] is [b] when an occurence of [x] was added. *)

  (*@ function singleton (x: 'a) : 'a t = add x empty *)
  (** [singleton x] is a bag containing one occurence of [x]. *)

  (*@ predicate mem (x: 'a) (b: 'a t) = multiplicity x b > 0 *)
  (** [mem x b] holds iff [b] contains [x] at least once. *)

  (*@ function remove (x: 'a) (b: 'a t) : 'a t *)
  (*@ axiom remove_mult_x : forall b x.
       multiplicity x (remove x b) = max 0 (multiplicity x b - 1) *)
  (*@ axiom remove_mult_neg_x : forall x y b. x <> y ->
      multiplicity y (remove x b) = multiplicity y b *)
  (** [remove x b] is [b] where an occurence of [x] was removed. *)

  (*@ function union (b b': 'a t) : 'a t *)
  (*@ axiom union_all : forall b b' x.
        max (multiplicity x b) (multiplicity x b') = multiplicity x (union b b') *)
  (** [union b b'] is a bag [br] where for all element [x],
      [multiplicity x br = max
      (multiplicity x b) (multiplicity x b')]. *)

  (*@ function sum (b b': 'a t) : 'a t *)
  (*@ axiom sum_all : forall b b' x.
        multiplicity x b + multiplicity x b' = multiplicity x (sum b b') *)
  (** [sum b b'] is a bag [br] where for all element [x],
      [multiplicity x br =
      (multiplicity x b) + (multiplicity x b')]. *)

  (*@ function inter (b b': 'a t) : 'a t *)
  (*@ axiom inter_all : forall b b' x.
        min (multiplicity x b) (multiplicity x b') = multiplicity x (inter b b') *)
  (** [inter b b'] is a bag [br] where for all element [x],
      [multiplicity x br =
      min (multiplicity x b) (multiplicity x b')]. *)

  (*@ function diff (b b': 'a t) : 'a t *)
  (*@ axiom diff_all : forall b b' x.
        max 0 (multiplicity x b - multiplicity x b') = multiplicity x (diff b b') *)

  (** [diff b b'] is a bag [br] where for all element [x],
      [multiplicity x br =
      max 0 (multiplicity x b - multiplicity x b')]. *)

  (*@ predicate disjoint (b b': 'a t) = forall x. mem x b -> not (mem x b') *)
  (** [disjoint b b'] holds iff [b] and [b'] have no element in common. *)

  (*@ predicate subset (b b': 'a t) = forall x. multiplicity x b <= multiplicity x b' *)
  (** [subset b b'] holds iff for all element [x],
      [multiplicity x b <= multiplicity x b']. *)

  (*@ function filter (f: 'a -> bool) (b: 'a t) : 'a t *)
  (*@ axiom filter_mem : forall b x f. f x ->
        multiplicity x (filter f b) = multiplicity x b *)
  (*@ axiom filter_mem_neg :
        forall b x f. not (f x) -> multiplicity x (filter f b) = 0 *)
  (** [filter f b] is the bag of all elements in [b] that satisfy [f]. *)

  (*@ function cardinal (b: 'a t) : integer  *)
  (** [cardinal b] is the total number of elements in [b], all occurrences being
      counted. *)
  (*@ predicate finite (b : 'a t) = exists s. forall x. mem x b -> Sequence.mem x s *)

  (* cardinality axioms finiteness hypothesis *)

  (*@ axiom card_nonneg :
       forall b. cardinal b >= 0 *)

  (*@ axiom card_empty : cardinal empty = 0 *)

  (*@ axiom card_singleton :
       forall x. cardinal (singleton x) = 1 *)

  (*@ axiom card_union :
       forall b1 b2. finite b1 -> finite b2 -> cardinal (union b1 b2) = cardinal b1 + cardinal b2 *)

  (*@ axiom card_add :
      forall x b. finite b -> cardinal (add x b) = cardinal b + 1 *)

  (*@ axiom card_map :
      forall f b. finite b -> cardinal (filter f b) <= cardinal b *)

  (*@ function of_seq (s: 'a Sequence.t) : 'a t *)
  (*@ axiom of_seq_multiplicity : forall s x. Sequence.multiplicity x s = multiplicity x (of_seq s) *)
end

(** {1 Sets} *)

(*@ function ({}) : 'a set *)
(** [{}] is the empty set. *)

module Set : sig
  (*@ type 'a t = 'a set *)
  (** An alias for ['a set]. *)

  (*@ predicate mem (x: 'a) (s: 'a t) *)
  (** [mem x s] is [x ∈ s]. *)

  (*@ function empty : 'a t *)
  (*@ axiom empty_mem : forall x. not (mem x empty) *)
  (** [empty] is [∅]. *)

  (*@ function add (x: 'a) (s: 'a t) : 'a t *)
  (*@ axiom add_mem : forall s x. mem x (add x s) *)
  (*@ axiom add_mem_neq : forall s x y. x <> y -> (mem x s <-> mem x (add y s)) *)
  (** [add x s] is [s ∪ {x}]. *)

  (*@ function singleton (x: 'a) : 'a t = add x empty  *)
  (** [singleton x] is [{x}]. *)

  (*@ function remove (x: 'a) (s: 'a t) : 'a t *)
  (*@ axiom remove_mem : forall s x. not (mem x (remove x s)) *)
  (*@ axiom remove_mem_neq : forall s x y. x <> y -> (mem x s <-> mem x (remove y s)) *)
  (** [remove x s] is [s ∖ {x}]. *)

  (*@ function union (s s': 'a t) : 'a t *)
  (*@ axiom union_mem : forall s s' x. (mem x s || mem x s') -> mem x (union s s') *)
  (*@ axiom union_mem_neg : forall s s' x.
        not (mem x s) -> not (mem x s') -> not (mem x (union s s')) *)
  (** [union s s'] is [s ∪ s']. *)

  (*@ function inter (s s': 'a t) : 'a t *)
  (*@ axiom inter_mem : forall s s' x. mem x s -> mem x s' -> mem x (inter s s') *)
  (*@ axiom inter_mem_neq : forall s s' x. not (mem x s || mem x s') -> not mem x (inter s s') *)

  (** [inter s s'] is [s ∩ s']. *)

  (*@ predicate disjoint (s s': 'a t) = inter s s' = empty *)
  (** [disjoint s s'] is [s ∩ s' = ∅]. *)

  (*@ function diff (s s': 'a t) : 'a t *)
  (*@ axiom diff_mem : forall s s' x. mem x s' -> not (mem x (diff s s')) *)
  (*@ axiom diff_mem_fst : forall s s' x. not (mem x s') -> (mem x s <-> mem x (diff s s')) *)
  (** [diff s s'] is [s ∖ s']. *)

  (*@ predicate subset (s s': 'a t) = forall x. mem x s -> mem x s' *)
  (** [subset s s'] is [s ⊂ s']. *)

  (*@ function map (f: 'a -> 'b) (s: 'a t) : 'b t *)
  (*@ axiom set_map : forall f s x.
        mem x (map f s) <-> (exists y. f y = x && mem y s) *)
  (** [map f s] is a fresh set which elements are [f x1 ... f xN], where
      [x1 ... xN] are the elements of [s]. *)

  (*@ function cardinal (s: 'a t) : integer *)
  (** [cardinal s] is the number of elements in [s]. *)

  (*@ predicate finite (s : 'a t) =
        exists seq. forall x. mem x s -> Sequence.mem x seq *)

  (*@ axiom cardinal_nonneg : forall s. cardinal s >= 0 *)
  (*@ axiom cardinal_empty : cardinal empty = 0 *)
  (*@ axiom cardinal_remove : forall s x. finite s ->
       if mem x s
         then cardinal (remove x s) = cardinal s - 1
         else cardinal (remove x s) = cardinal s*)
  (*@ axiom cardinal_add : forall s x. finite s ->
       if mem x s
          then cardinal (add x s) = cardinal s
          else cardinal (add x s) = cardinal s + 1*)

  (*@ function of_seq (s: 'a Sequence.t) : 'a t *)
  (*@ axiom of_seq_set : forall x s.
       Sequence.mem x s <-> mem x (of_seq s) *)

  (*@ function fold (f : 'a -> 'b -> 'b) (s : 'a t) (acc : 'b) : 'b *)
  (*<- to be removed*)
end

module Map : sig
  (** Infinite maps from keys of type ['a] to values of type ['b] *)
  (*@ type ('a, 'b) t = ('a, 'b) map*)

  (*@ function domain (default : 'b) (m : ('a, 'b) t) : 'a Set.t *)
  (*@ axiom domain_mem :
        forall x m default. m x <> default -> Set.mem x (domain default m) *)
end

(* The following modules are deprecated and only exist to ensure the tests pass. In the future, assuming this branch is merged, they should be removed and the tests changed*)

module Array : sig
  (*@ function get (a : 'a array) (i : integer) : 'a *)

  (*@ function length (a : 'a array) : integer *)
  (*@ function to_seq (a : 'a array) : 'a sequence *)
  (*@ coercion *)
  (*@ predicate permut (a1 : 'a array) (b1 : 'a array) *)
  (*@ predicate permut_sub (a1 : 'a array) (a2 : 'a array) (i : integer) (j : integer) *)

end

module List : sig
  (*@ function fold_left (f : 'b -> 'a -> 'b) (acc : 'b) (l : 'a list) : 'b *)
  (*@ predicate _exists (f : 'a -> bool) (l : 'a list) *)
  (*@ function length (l : 'a list) : integer *)
  (*@ function nth (l : 'a list) (i : integer) : 'a *)
  (*@ predicate mem (x : 'a) (l : 'a list) *)
  (*@ function map (f : 'a -> 'b) (l : 'a list) : 'b list *)
end

module Order : sig
  (*@ predicate is_pre_order (f: 'a -> 'a -> int) *)
end
(*@ type 'a ref *)
(*@ function (!_) (r : 'a ref) : 'a *)
(*@ function logand (n1 : integer) (n2: integer) : integer *)
(*@ function integer_of_int (n : int) : integer *)
(*@ coercion *)
