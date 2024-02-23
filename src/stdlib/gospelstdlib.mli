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

(*@ type 'a bag = 'a -> integer *)
(** The type for finite unordered multisets. *)

(*@ type 'a set = 'a -> bool *)
(** The type for finite unordered sets. *)

(*@ type ('a, 'b) map = 'a -> 'b *)
(** The type for infinite maps *)

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

(** {2 Machine integers}

    There is a coercion from type [int] to type [integer], so that Gospel
    specifications can be written using type [integer] only, and yet use OCaml's
    variables of type [int]. The Gospel typechecker will automatically apply
    [integer_of_int] whenever necessary. *)

(*@ function integer_of_int (x: int) : integer *)
(*@ coercion *)

(*@ function max_int : integer *)
(*@ function min_int : integer *)
(** {1 Sequences} *)

(*@ function (++) (s s': 'a sequence) : 'a sequence *)
(** [s ++ s'] is the sequence [s] followed by the sequence [s']. *)

(*@ function ([_]) (s: 'a sequence) (i: integer): 'a *)

(** [s[i]] is the [i]th element of the sequence [s]. *)

(*@ function ([->]) (f: 'a -> 'b) (x:'a) (y: 'b) : 'a -> 'b = 
      fun arg -> if arg = x then y else f x *)

module Sequence : sig
  (*@ type 'a t = 'a sequence *)
  (** An alias for {!sequence} *)

  (*@ function length (s: 'a t): integer *)
  (*@ axiom length_nonneg : forall s. 0 <= length s *)
  (*@ axiom append_length : forall s s'. length (s++s') = length s'+ length s *)
  (*@ axiom append_elems_left :  forall s s' i. i <= 0 < length s -> (s ++ s')[i] = s[i] *)
  (*@ axiom append_elems_right : 
     forall s s' i. 
      length s <= i < length s + length s' -> 
      (s ++ s')[i + length s] = s'[i] *)
  
  (*@ function ([_.._]) (s: 'a sequence) (i1: integer) (i2: integer): 'a sequence *)
  (*@ axiom subseq : forall s i i1 i2. i1 <= i < i2 -> s[i] = (s[i1 .. i2])[i-i1] *)

  (*@ function ([_..]) (s: 'a sequence) (i: integer): 'a sequence = s[i .. length s] *)
  (*@ function ([.._]) (s: 'a sequence) (i: integer): 'a sequence = s[0 .. i] *)

  (*@ function init (n: integer) (f: integer -> 'a) : 'a t *)
  (*@ axiom init_length : forall n f. length (init n f) = n *)
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

  (*@ predicate mem (x: 'a) (s: 'a t) = exists i. s[i] = x *)
  (** [mem s x] holds iff [x] is in [s]. *)

  (*@ function map (f: 'a -> 'b) (s: 'a t) : 'b t *)
  (*@ axiom map_elems : forall i f s. (map f s)[i] = f (s[i]) *)
  (** [map f s] is a sequence whose elements are the elements of [s],
      transformed by [f]. *)

  (*@ function filter (f: 'a -> bool) (s: 'a t) : 'a t *)
  (*@ axiom filter_elems : 
        forall f s i. 0 <= i < length (filter f s) -> 
          f ((filter f s)[i]) && mem ((filter f s)[i]) s *)
  (** [filter f s] is a sequence whose elements are the elements of [s], that *)

  (*@ function get (s: 'a t) (i: integer) : 'a = s[i] *)
  (** [get s i] is [s[i]]. *)

  (*@ function set (s: 'a t) (i: integer) (x: 'a): 'a t *)
  (*@ axiom set_elem : forall s i x. (set s i x)[i] = x *)
  (*@ axiom set_elem_other : forall s i1 i2 x. i1 <> i2 -> (set s i1 x)[i2] = s[i2] *)

  (** [set s i x] is the sequence [s] where the [i]th element is [x]. *)

  (*@ function of_list (l : 'a list) : 'a t *)
  (*@ coercion *)
  
  (*@ function rev (s: 'a t) : 'a t *)
  (*@ axiom rev_length : forall s. length s = length (rev s) *)
  (*@ axiom rev_elems  : forall i s. 0 <= i < length s -> s[i] = (rev s)[length s - i - 1] *)

  (** [rev s] is the sequence containing the same elements as [s], in reverse
      order. *)

  (*@ function rec fold (f: 'a -> 'b -> 'a) (acc: 'a) (s: 'b sequence) : 'a = 
         if s = empty 
           then acc
           else fold f (f acc (hd s)) (tl s) *)
  (** [fold f acc s] is [f (... (f (f acc s[0]) s[1]) ...) s[n-1]], where
      [n] is the length of [s]. *)
end
(** {1 Bags} *)

module Bag : sig
  (*@ type 'a t = 'a bag *)
  (** An alias for ['a bag]. *)

  (*@ function occurrences (x: 'a) (b: 'a t): integer = b x *)
  (** [occurrences x b] is the number of occurrences of [x] in [s]. *)
  
  (*@ function empty : 'a t = fun _ -> 0 *)
  (** [empty] is the empty bag. *)

  (*@ predicate is_empty (b: 'a t) =  b = empty *)
  (** [is_empty b] is [b = empty]. *)

  (*@ predicate mem (x: 'a) (b: 'a t) = b x > 0*)
  (** [mem x b] holds iff [b] contains [x] at least once. *)

  (*@ function add (x: 'a) (b: 'a t) : 'a t = 
      b[x -> b x + 1] *)
  (** [add x b] is [b] when an occurence of [x] was added. *)

  (*@ function singleton (x: 'a) : 'a t = 
        fun arg -> if arg = x then 1 else 0 *)
  (** [singleton x] is a bag containing one occurence of [x]. *)

  (*@ function remove (x: 'a) (b: 'a t) : 'a t = b[x -> max 0 (b x - 1)] *)
  (** [remove x b] is [b] where an occurence of [x] was removed. *)

  (*@ function union (b b': 'a t) : 'a t = 
        fun x -> max (b x) (b' x) *)
  (** [union b b'] is a bag [br] where for all element [x],
      [occurences x br = max
      (occurences x b) (occurences x b')]. *)

  (*@ function sum (b b': 'a t) : 'a t = 
        fun x -> b x + b' x *)
  (** [sum b b'] is a bag [br] where for all element [x],
      [occurences x br =
      (occurences x b) + (occurences x b')]. *)

  (*@ function inter (b b': 'a t) : 'a t = 
         fun x -> min (b x) (b' x) *)
  (** [inter b b'] is a bag [br] where for all element [x],
      [occurences x br =
      min (occurences x b) (occurences x b')]. *)

  (*@ predicate disjoint (b b': 'a t) = 
        forall x. mem x b -> not (mem x b')
   *)
  (** [disjoint b b'] holds iff [b] and [b'] have no element in common. *)

  (*@ function diff (b b': 'a t) : 'a t = 
        fun arg -> max 0 (b arg - b' arg) *)
  (** [diff b b'] is a bag [br] where for all element [x],
      [occurences x br =
      max 0 (occurences x b - occurences x b')]. *)

  (*@ predicate subset (b b': 'a t) = 
        forall x. b x <= b' x *)
  (** [subset b b'] holds iff for all element [x],
      [occurences x b <= occurences x b']. *)

  (*@ function map (f: 'a -> 'b) (b: 'a t) : 'b t *)
  (*@ axiom map_def : forall x y f b. f x = y -> f x = (map f b) y *)
  
  (** [map f b] is a fresh bag which elements are [f x1 ... f xN], where
      [x1 ... xN] are the elements of [b]. *)

  (*@ function filter (f: 'a -> bool) (b: 'a t) : 'a t *)
  (*@ axiom filter_mem : forall b x f. f x -> map f b x = b x *)
  (*@ axiom filter_mem_neg : forall b x f. not (f x) -> map f b x = 0 *)
  
  (** [filter f b] is the bag of all elements in [b] that satisfy [f]. *)

  (*@ function cardinal (b: 'a t) : integer  *)
  (** [cardinal b] is the total number of elements in [b], all occurrences being
      counted. *)

  (*@ axiom card_nonneg : 
       forall b. cardinal b >= 0 *)
  
  (*@ axiom card_empty : 
        forall b. empty = b <-> cardinal b = 0 *)

  (*@ axiom card_singleton : 
       forall x. cardinal (singleton x) = 1 *)

  (*@ axiom card_union : 
       forall b1 b2. cardinal (union b1 b2) = cardinal b1 + cardinal b2 *)

  (*@ axiom card_add : 
      forall x b. cardinal (add b x) = cardinal b + 1 *)

  (*@ axiom card_map : 
      forall f b. cardinal (map f b) <= cardinal b *)
  
  (*@ function of_seq (s: 'a Sequence.t) : 'a t *)
  (*@ axiom of_seq_mem : forall s x. Sequence.mem x s -> mem x (of_seq s) *)
  (*@ axiom of_seq_mem_neg : forall s x. not Sequence.mem x s -> not (mem x (of_seq s)) *)
  
end

(** {1 Sets} *)

(*@ function ({}) : 'a set *)
(** [{}] is the empty set. *)

module Set : sig
  (*@ type 'a t = 'a set *)
  (** An alias for ['a set]. *)
  
  (*@ function empty : 'a t = fun _ -> false *)
  (** [empty] is [∅]. *)

  (*@ predicate is_empty (s: 'a t) = s = empty *)
  (** [is_empty s] is [s = ∅]. *)

  (*@ predicate mem (x: 'a) (s: 'a t) = s x *)
  (** [mem x s] is [x ∈ s]. *)

  (*@ function add (x: 'a) (s: 'a t) : 'a t = s[x -> true] *)
  (** [add x s] is [s ∪ {x}]. *)

  (*@ function singleton (x: 'a) : 'a t = fun arg -> arg = x  *)
  (** [singleton x] is [{x}]. *)

  (*@ function remove (x: 'a) (s: 'a t) : 'a t = s[x -> false] *)
  (** [remove x s] is [s ∖ {x}]. *)

  (*@ function union (s s': 'a t) : 'a t = fun arg -> s arg || s' arg *)
  (** [union s s'] is [s ∪ s']. *)

  (*@ function inter (s s': 'a t) : 'a t = fun arg -> s arg && s' arg *)
  (** [inter s s'] is [s ∩ s']. *)

  (*@ predicate disjoint (s s': 'a t) = forall x. s x -> not s' x *)
  (** [disjoint s s'] is [s ∩ s' = ∅]. *)

  (*@ function diff (s s': 'a t) : 'a t = fun arg -> s arg && not (s' arg) *)
  (** [diff s s'] is [s ∖ s']. *)

  (*@ predicate subset (s s': 'a t) = forall x. s x -> s' x *)
  (** [subset s s'] is [s ⊂ s']. *)
  
  (*@ function map (f: 'a -> 'b) (s: 'a t) : 'b t *)
  (*@ axiom set_map : forall f s x y. f x = y -> f x <-> (map f s) y*)
  (** [map f s] is a fresh set which elements are [f x1 ... f xN], where
      [x1 ... xN] are the elements of [s]. *)

  (*@ function cardinal (s: 'a t) : integer *)
  (** [cardinal s] is the number of elements in [s]. *)

  (*@ predicate finite (s : 'a t) = exists seq. forall x. s x -> Sequence.mem x seq *)
  
  (*@ axiom cardinal_nonneg : forall s. finite s -> cardinal s >= 0 *)
  (*@ axiom cardinal_empty : forall s. finite s -> is_empty s <-> cardinal s = 0 *)
  (*@ axiom cardinal_remove : forall s x. finite s -> 
       if mem x s 
         then cardinal (remove x s) = cardinal s - 1
         else cardinal (remove x s) = cardinal s*)
  (*@ axiom cardinal_add : forall s x. finite s ->
       if mem x s 
          then cardinal (add x s) = cardinal s + 1
          else cardinal (add x s) = cardinal s*)
  
  (*@ function of_seq (s: 'a Sequence.t) : 'a t *)

  (*@ axiom of_seq_set : forall x s. Sequence.mem x s -> of_seq s x *)
  (*@ axiom of_seq_set_neg : forall x s. Sequence.mem x s -> not (of_seq s x) *)

end

module Map : sig
  (** Infinite maps from keys of type ['a] to values of type ['b] *)
  (*@ type ('a, 'b) t = ('a, 'b) map*)

  (*@ function domain (m : ('a, 'b) t) (e : 'b) : 'a Set.t =
         fun arg -> m arg <> e *)
end
