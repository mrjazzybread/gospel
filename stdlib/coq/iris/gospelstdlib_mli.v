Require Import Gospel.primitives Stdlib.ZArith.BinInt stdpp.base.

Require Import
  Stdlib.Floats.Floats
  Stdlib.ZArith.BinIntDef
  Stdlib.Strings.Ascii.

Local Open Scope Z_scope.

Module Declarations.

  Class _set_sig  := { set : forall (A : Type), Type }.

  Class _sequence_sig  := { sequence : forall (A : Type), Type }.

  Class _bag_sig  := { bag : forall (A : Type), Type }.

  Class _option_sig  := { option : forall (A : Type), Type }.

  Class _fin_map_sig  := { fin_map : forall (K : Type) (V : Type), Type }.

  Class _Some_sig `{@_option_sig}  := {
    Some :
      forall {A}, forall `{Inhabited A}, A -> (option A)
  }.

  Class _None_sig `{@_option_sig}  := {
    None :
      forall {A}, forall `{Inhabited A}, option A
  }.

  Definition map (A : Type) (B : Type) : Type := A -> B.

  Class _succ_sig  := { succ : integer -> integer }.

  Class _pred_sig  := { pred : integer -> integer }.

  Class ____mod_sig  := { ___mod : integer -> integer -> integer }.

  Class _pow_sig  := { pow : integer -> integer -> integer }.

  Class _abs_sig  := { abs : integer -> integer }.

  Class _min_sig  := { min : integer -> integer -> integer }.

  Class _max_sig  := { max : integer -> integer -> integer }.

  Class ___app_sig `{@_sequence_sig}  := {
    __app :
      forall {A}, forall `{Inhabited A}, (sequence A) ->
      (sequence A) ->
      sequence A
  }.

  Class ___seq_get_sig `{@_sequence_sig}  := {
    __seq_get :
      forall {A}, forall `{Inhabited A}, (sequence A) -> integer -> A
  }.

  Class ___seq_sub_sig `{@_sequence_sig}  := {
    __seq_sub :
      forall {A}, forall `{Inhabited A}, (sequence A) ->
      integer ->
      integer
      ->
      (sequence A)
  }.

  Class ___seq_sub_l_sig `{@_sequence_sig}  := {
    __seq_sub_l :
      forall {A}, forall `{Inhabited A}, (sequence A) -> integer -> sequence A
  }.

  Class ___seq_sub_r_sig `{@_sequence_sig}  := {
    __seq_sub_r :
      forall {A}, forall `{Inhabited A}, (sequence A) -> integer -> sequence A
  }.

  Class _monoid_sig  := {
    monoid :
      forall {A}, forall `{Inhabited A}, (A -> A -> A) -> A -> prop
  }.

  Class _monoid_def_sig `{@_monoid_sig}  := {
    monoid_def :
      forall {A}, forall `{Inhabited A}, forall
      (f : A -> A -> A)
      (neutral : A),
      (monoid f neutral)
      <->
      ((forall (x : A), ((f neutral x) = (f x neutral)) /\ ((f x neutral) = x))
      /\
      (forall (x : A) (y : A) (z : A), (f x (f y z)) = (f (f x y) z)))
  }.

  Class _comm_monoid_sig  := {
    comm_monoid :
      forall {A}, forall `{Inhabited A}, (A -> A -> A) -> A -> prop
  }.

  Class _comm_monoid_def_sig `{@_comm_monoid_sig} `{@_monoid_sig}  := {
    comm_monoid_def :
      forall {A}, forall `{Inhabited A}, forall
      (f : A -> A -> A)
      (neutral : A),
      (comm_monoid f neutral)
      <->
      ((monoid f neutral) /\ (forall (x : A) (y : A), (f x y) = (f y x)))
  }.

  Module Sequence.

    Definition t `{@_sequence_sig} (A : Type) : Type := sequence A.

    Class _length_sig `{@_sequence_sig}  := {
      length :
        forall {A}, forall `{Inhabited A}, (t A) -> integer
    }.

    Class _in_range_sig `{@_sequence_sig}  := {
      in_range :
        forall {A}, forall `{Inhabited A}, (t A) -> integer -> prop
    }.

    Class _in_range_def_sig `{@_sequence_sig}
    `{@_in_range_sig _}
    `{@_length_sig _}
     := {
      in_range_def :
        forall {A}, forall `{Inhabited A}, forall
        (s : sequence A)
        (i : integer),
        (in_range s i) <-> ((0 <= i) /\ (i < (length s)))
    }.

    Class _length_nonneg_sig `{@_sequence_sig} `{@_length_sig _}  := {
      length_nonneg :
        forall {A}, forall `{Inhabited A}, forall (s : sequence A), 0
        <=
        (length s)
    }.

    Class _subseq_l_sig `{@_sequence_sig}
    `{@_in_range_sig _}
    `{@___seq_sub_l_sig _}
    `{@___seq_sub_sig _}
    `{@_length_sig _}
     := {
      subseq_l :
        forall {A}, forall `{Inhabited A}, forall
        (s : sequence A)
        (i : integer),
        (in_range s i) -> ((__seq_sub_l s i) = (__seq_sub s i (length s)))
    }.

    Class _subseq_r_sig `{@_sequence_sig}
    `{@_in_range_sig _}
    `{@___seq_sub_r_sig _}
    `{@___seq_sub_sig _}
     := {
      subseq_r :
        forall {A}, forall `{Inhabited A}, forall
        (s : sequence A)
        (i : integer),
        (in_range s i) -> ((__seq_sub_r s i) = (__seq_sub s 0 i))
    }.

    Class _subseq_sig `{@_sequence_sig}
    `{@_length_sig _}
    `{@___seq_get_sig _}
    `{@___seq_sub_sig _}
     := {
      subseq :
        forall {A}, forall `{Inhabited A}, forall
        (s : sequence A)
        (i : integer)
        (i1 : integer)
        (i2 : integer),
        ((0 <= i1) /\ ((i1 <= i) /\ ((i < i2) /\ (i2 <= (length s)))))
        ->
        ((__seq_get s i) = (__seq_get (__seq_sub s i1 i2) (i - i1)))
    }.

    Class _subseq_len_sig `{@_sequence_sig}
    `{@_length_sig _}
    `{@___seq_sub_sig _}
     := {
      subseq_len :
        forall {A}, forall `{Inhabited A}, forall
        (s : sequence A)
        (i1 : integer)
        (i2 : integer),
        ((0 <= i1) /\ ((i1 <= i2) /\ (i2 < (length s))))
        ->
        ((length (__seq_sub s i1 i2)) = (i2 - i1))
    }.

    Class _empty_sig `{@_sequence_sig}  := {
      empty :
        forall {A}, forall `{Inhabited A}, t A
    }.

    Class ___empty_sig `{@_sequence_sig}  := {
      __empty :
        forall {A}, forall `{Inhabited A}, t A
    }.

    Class _empty_length_sig `{@_sequence_sig}
    `{@_length_sig _}
    `{@___empty_sig _}
     := {
      empty_length :
        forall {A}, forall `{Inhabited A}, (length (A := A) __empty) = 0
    }.

    Class _init_sig `{@_sequence_sig}  := {
      init :
        forall {A}, forall `{Inhabited A}, integer -> (integer -> A) -> t A
    }.

    Class _init_length_sig `{@_sequence_sig}
    `{@_length_sig _}
    `{@_init_sig _}
     := {
      init_length :
        forall {A}, forall `{Inhabited A}, forall
        (n : integer)
        (f : integer -> A),
        (n >= 0) -> ((length (init n f)) = n)
    }.

    Class _init_elems_sig `{@_sequence_sig}
    `{@___seq_get_sig _}
    `{@_init_sig _}
     := {
      init_elems :
        forall {A}, forall `{Inhabited A}, forall
        (n : integer)
        (f : integer -> A)
        (i : integer),
        ((0 <= i) /\ (i < n)) -> ((__seq_get (init n f) i) = (f i))
    }.

    Class _singleton_sig `{@_sequence_sig}  := {
      singleton :
        forall {A}, forall `{Inhabited A}, A -> (t A)
    }.

    Class _singleton_def_sig `{@_sequence_sig}
    `{@_singleton_sig _}
    `{@_init_sig _}
     := {
      singleton_def :
        forall {A}, forall `{Inhabited A}, forall (x : A) (f : integer -> A), ((f 0)
        =
        x)
        ->
        ((singleton x) = (init 1 f))
    }.

    Class _cons_sig `{@_sequence_sig}  := {
      cons :
        forall {A}, forall `{Inhabited A}, A -> (t A) -> t A
    }.

    Class _cons_def_sig `{@_sequence_sig}
    `{@_cons_sig _}
    `{@___app_sig _}
    `{@_singleton_sig _}
     := {
      cons_def :
        forall {A}, forall `{Inhabited A}, forall (x : A) (s : sequence A), (cons x s)
        =
        (__app (singleton x) s)
    }.

    Class _snoc_sig `{@_sequence_sig}  := {
      snoc :
        forall {A}, forall `{Inhabited A}, (t A) -> A -> t A
    }.

    Class _snoc_def_sig `{@_sequence_sig}
    `{@_snoc_sig _}
    `{@___app_sig _}
    `{@_singleton_sig _}
     := {
      snoc_def :
        forall {A}, forall `{Inhabited A}, forall (s : sequence A) (x : A), (snoc s x)
        =
        (__app s (singleton x))
    }.

    Class _hd_sig `{@_sequence_sig}  := {
      hd :
        forall {A}, forall `{Inhabited A}, (t A) -> A
    }.

    Class _hd_def_sig `{@_sequence_sig} `{@_hd_sig _} `{@___seq_get_sig _}  := {
      hd_def :
        forall {A}, forall `{Inhabited A}, forall (s : sequence A), (hd s)
        =
        (__seq_get s 0)
    }.

    Class _tl_sig `{@_sequence_sig}  := {
      tl :
        forall {A}, forall `{Inhabited A}, (t A) -> (t A)
    }.

    Class _tl_def_sig `{@_sequence_sig} `{@_cons_sig _} `{@_tl_sig _}  := {
      tl_def :
        forall {A}, forall `{Inhabited A}, forall
        (s : sequence A)
        (t : sequence A)
        (x : A),
        (s = (cons x t)) -> ((tl s) = t)
    }.

    Class _append_sig `{@_sequence_sig}  := {
      append :
        forall {A}, forall `{Inhabited A}, (t A) -> (t A) -> t A
    }.

    Class _append_def_sig `{@_sequence_sig}
    `{@_append_sig _}
    `{@___app_sig _}
     := {
      append_def :
        forall {A}, forall `{Inhabited A}, forall
        (s1 : sequence A)
        (s2 : sequence A),
        (append s1 s2) = (__app s1 s2)
    }.

    Class _append_length_sig `{@_sequence_sig}
    `{@_length_sig _}
    `{@___app_sig _}
     := {
      append_length :
        forall {A}, forall `{Inhabited A}, forall
        (s : sequence A)
        (s' : sequence A),
        (length (__app s s')) = ((length s) + (length s'))
    }.

    Class _append_elems_left_sig `{@_sequence_sig}
    `{@_in_range_sig _}
    `{@___seq_get_sig _}
    `{@___app_sig _}
     := {
      append_elems_left :
        forall {A}, forall `{Inhabited A}, forall
        (s : sequence A)
        (s' : sequence A)
        (i : integer),
        (in_range s i) -> ((__seq_get (__app s s') i) = (__seq_get s i))
    }.

    Class _append_elems_right_sig `{@_sequence_sig}
    `{@_length_sig _}
    `{@___seq_get_sig _}
    `{@___app_sig _}
     := {
      append_elems_right :
        forall {A}, forall `{Inhabited A}, forall
        (s : sequence A)
        (s' : sequence A)
        (i : integer),
        (((length s) <= i) /\ (i < ((length s) + (length s'))))
        ->
        ((__seq_get (__app s s') i) = (__seq_get s' (i - (length s))))
    }.

    Class ___multiplicity_sig `{@_sequence_sig}  := {
      __multiplicity :
        forall {A}, forall `{Inhabited A}, A -> (t A) -> integer
    }.

    Class _mult_empty_sig `{@_sequence_sig}
    `{@___multiplicity_sig _}
    `{@___empty_sig _}
     := {
      mult_empty :
        forall {A}, forall `{Inhabited A}, forall (x : A), (__multiplicity x __empty)
        =
        0
    }.

    Class _mult_cons_sig `{@_sequence_sig}
    `{@___multiplicity_sig _}
    `{@_cons_sig _}
     := {
      mult_cons :
        forall {A}, forall `{Inhabited A}, forall (s : sequence A) (x : A), (1
        +
        (__multiplicity x s))
        =
        (__multiplicity x (cons x s))
    }.

    Class _mult_cons_neutral_sig `{@_sequence_sig}
    `{@___multiplicity_sig _}
    `{@_cons_sig _}
     := {
      mult_cons_neutral :
        forall {A}, forall `{Inhabited A}, forall
        (s : sequence A)
        (x1 : A)
        (x2 : A),
        (x1 <> x2) -> ((__multiplicity x1 s) = (__multiplicity x1 (cons x2 s)))
    }.

    Class _mult_length_sig `{@_sequence_sig}
    `{@___multiplicity_sig _}
    `{@_length_sig _}
     := {
      mult_length :
        forall {A}, forall `{Inhabited A}, forall (x : A) (s : sequence A), (0
        <=
        (__multiplicity x s))
        /\
        ((__multiplicity x s) <= (length s))
    }.

    Class ___belongs_sig `{@_sequence_sig}  := {
      __belongs :
        forall {A}, forall `{Inhabited A}, A -> (t A) -> prop
    }.

    Class _mem_sig `{@_sequence_sig}  := {
      mem :
        forall {A}, forall `{Inhabited A}, A -> (t A) -> prop
    }.

    Class _mem_fun_def_sig `{@_sequence_sig}
    `{@___belongs_sig _}
    `{@_mem_sig _}
     := {
      mem_fun_def :
        forall {A}, forall `{Inhabited A}, forall (x : A) (s : sequence A), (__belongs x s)
        <->
        (mem x s)
    }.

    Class _mem_def_sig `{@_sequence_sig}
    `{@___belongs_sig _}
    `{@___multiplicity_sig _}
     := {
      mem_def :
        forall {A}, forall `{Inhabited A}, forall (s : sequence A), forall
        (x : A),
        (__belongs x s) <-> ((__multiplicity x s) > 0)
    }.

    Class ___neg_belongs_sig `{@_sequence_sig}  := {
      __neg_belongs :
        forall {A}, forall `{Inhabited A}, A -> (t A) -> prop
    }.

    Class _nmem_def_sig `{@_sequence_sig}
    `{@___neg_belongs_sig _}
    `{@___belongs_sig _}
     := {
      nmem_def :
        forall {A}, forall `{Inhabited A}, forall (s : sequence A) (x : A), (__neg_belongs x s)
        <->
        (not (__belongs x s))
    }.

    Class _Forall_sig `{@_sequence_sig}  := {
      Forall :
        forall {A}, forall `{Inhabited A}, (A -> prop) -> (sequence A) -> prop
    }.

    Class _forall_def_sig `{@_sequence_sig}
    `{@_Forall_sig _}
    `{@___belongs_sig _}
     := {
      forall_def :
        forall {A}, forall `{Inhabited A}, forall
        (p : A -> prop)
        (s : sequence A),
        (Forall p s) <-> (forall (x : A), (__belongs x s) -> (p x))
    }.

    Class _Exists_sig `{@_sequence_sig}  := {
      Exists :
        forall {A}, forall `{Inhabited A}, (A -> prop) -> (sequence A) -> prop
    }.

    Class _exists_def_sig `{@_sequence_sig}
    `{@_Exists_sig _}
    `{@___belongs_sig _}
     := {
      exists_def :
        forall {A}, forall `{Inhabited A}, forall
        (p : A -> prop)
        (s : sequence A),
        (Exists p s) <-> (exists (x : A), (__belongs x s) /\ (p x))
    }.

    Class _map_sig `{@_sequence_sig}  := {
      map :
        forall {A} {B}, forall `{Inhabited A} `{Inhabited B}, (A -> B) ->
        (t A) ->
        t B
    }.

    Class _map_elems_sig `{@_sequence_sig}
    `{@_in_range_sig _}
    `{@___seq_get_sig _}
    `{@_map_sig _}
     := {
      map_elems :
        forall {B} {A}, forall `{Inhabited B} `{Inhabited A}, forall
        (i : integer)
        (f : B -> A)
        (s : sequence B),
        (in_range s i) -> ((__seq_get (map f s) i) = (f (__seq_get s i)))
    }.

    Class _map_length_sig `{@_sequence_sig}
    `{@_length_sig _}
    `{@_map_sig _}
     := {
      map_length :
        forall {B} {A}, forall `{Inhabited B} `{Inhabited A}, forall
        (f : B -> A)
        (s : sequence B),
        (length (map f s)) = (length s)
    }.

    Class _filter_sig `{@_sequence_sig}  := {
      filter :
        forall {A}, forall `{Inhabited A}, (A -> prop) -> (t A) -> t A
    }.

    Class _filter_elems_sig `{@_sequence_sig}
    `{@___belongs_sig _}
    `{@_filter_sig _}
     := {
      filter_elems :
        forall {A}, forall `{Inhabited A}, forall
        (f : A -> prop)
        (s : sequence A)
        (x : A),
        (__belongs x s) -> (f x) -> __belongs x (filter f s)
    }.

    Class _filter_map_sig `{@_sequence_sig} `{@_option_sig}  := {
      filter_map :
        forall {A} {B}, forall `{Inhabited A} `{Inhabited B}, (A -> (option B)) ->
        (t A) ->
        t B
    }.

    Class _filter_map_elems_sig `{@_option_sig}
    `{@_sequence_sig}
    `{@_Some_sig _}
    `{@___belongs_sig _}
    `{@_filter_map_sig _ _}
     := {
      filter_map_elems :
        forall {B} {A}, forall `{Inhabited B} `{Inhabited A}, forall
        (f : B -> (option A))
        (s : sequence B)
        (y : A),
        (exists (x : B), ((f x) = (Some y)) /\ (__belongs x s))
        <->
        (__belongs y (filter_map f s))
    }.

    Class _get_sig `{@_sequence_sig}  := {
      get :
        forall {A}, forall `{Inhabited A}, (t A) -> integer -> A
    }.

    Class _get_def_sig `{@_sequence_sig}
    `{@_get_sig _}
    `{@___seq_get_sig _}
     := {
      get_def :
        forall {A}, forall `{Inhabited A}, forall
        (s : sequence A)
        (i : integer),
        (get s i) = (__seq_get s i)
    }.

    Class _set_sig `{@_sequence_sig}  := {
      set :
        forall {A}, forall `{Inhabited A}, (t A) -> integer -> A -> (t A)
    }.

    Class _set_elem_sig `{@_sequence_sig}
    `{@_in_range_sig _}
    `{@___seq_get_sig _}
    `{@_set_sig _}
     := {
      set_elem :
        forall {A}, forall `{Inhabited A}, forall
        (s : sequence A)
        (i : integer)
        (x : A),
        (in_range s i) -> ((__seq_get (set s i x) i) = x)
    }.

    Class _set_elem_other_sig `{@_sequence_sig}
    `{@_in_range_sig _}
    `{@___seq_get_sig _}
    `{@_set_sig _}
     := {
      set_elem_other :
        forall {A}, forall `{Inhabited A}, forall
        (s : sequence A)
        (i1 : integer)
        (i2 : integer)
        (x : A),
        (i1 <> i2) ->
        (in_range s i1) ->
        (in_range s i2)
        ->
        ((__seq_get (set s i1 x) i2) = (__seq_get s i2))
    }.

    Class _rev_sig `{@_sequence_sig}  := {
      rev :
        forall {A}, forall `{Inhabited A}, (t A) -> (t A)
    }.

    Class _rev_length_sig `{@_sequence_sig}
    `{@_length_sig _}
    `{@_rev_sig _}
     := {
      rev_length :
        forall {A}, forall `{Inhabited A}, forall (s : sequence A), (length s)
        =
        (length (rev s))
    }.

    Class _rev_elems_sig `{@_sequence_sig}
    `{@_in_range_sig _}
    `{@___seq_get_sig _}
    `{@_rev_sig _}
    `{@_length_sig _}
     := {
      rev_elems :
        forall {A}, forall `{Inhabited A}, forall
        (i : integer)
        (s : sequence A),
        (in_range s i)
        ->
        ((__seq_get (rev s) i) = (__seq_get s (((length s) - 1) - i)))
    }.

    Class _extensionality_sig `{@_sequence_sig}
    `{@_length_sig _}
    `{@_in_range_sig _}
    `{@___seq_get_sig _}
     := {
      extensionality :
        forall {A}, forall `{Inhabited A}, forall
        (s1 : sequence A)
        (s2 : sequence A),
        ((length s1) = (length s2)) ->
        (forall (i : integer), (in_range s1 i)
        ->
        ((__seq_get s1 i) = (__seq_get s2 i))) ->
        s1
        =
        s2
    }.

    Class _fold_left_sig `{@_sequence_sig}  := {
      fold_left :
        forall {A} {B}, forall `{Inhabited A} `{Inhabited B}, (A -> B -> A) ->
        A ->
        (t B)
        ->
        A
    }.

    Class _fold_left_empty_sig `{@_sequence_sig}
    `{@_fold_left_sig _}
    `{@___empty_sig _}
     := {
      fold_left_empty :
        forall {B} {A}, forall `{Inhabited B} `{Inhabited A}, forall
        (f : A -> B -> A)
        (acc : A),
        (fold_left f acc __empty) = acc
    }.

    Class _fold_left_cons_sig `{@_sequence_sig}
    `{@_fold_left_sig _}
    `{@_cons_sig _}
     := {
      fold_left_cons :
        forall {B} {A}, forall `{Inhabited B} `{Inhabited A}, forall
        (f : A -> B -> A)
        (acc : A)
        (x : B)
        (l : sequence B),
        (fold_left f acc (cons x l)) = (fold_left f (f acc x) l)
    }.

    Class _fold_right_sig `{@_sequence_sig}  := {
      fold_right :
        forall {A} {B}, forall `{Inhabited A} `{Inhabited B}, (A -> B -> B) ->
        (t A) ->
        B
        ->
        B
    }.

    Class _fold_right_empty_sig `{@_sequence_sig}
    `{@_fold_right_sig _}
    `{@___empty_sig _}
     := {
      fold_right_empty :
        forall {B} {A}, forall `{Inhabited B} `{Inhabited A}, forall
        (acc : A)
        (f : B -> A -> A),
        (fold_right f __empty acc) = acc
    }.

    Class _fold_right_cons_sig `{@_sequence_sig}
    `{@_fold_right_sig _}
    `{@_cons_sig _}
     := {
      fold_right_cons :
        forall {B} {A}, forall `{Inhabited B} `{Inhabited A}, forall
        (acc : A)
        (f : B -> A -> A)
        (x : B)
        (l : sequence B),
        (fold_right f (cons x l) acc) = (f x (fold_right f l acc))
    }.

    Class _permut_sig `{@_sequence_sig}  := {
      permut :
        forall {A}, forall `{Inhabited A}, (t A) -> (t A) -> prop
    }.

    Class _permut_mem_sig `{@_sequence_sig}
    `{@_permut_sig _}
    `{@___belongs_sig _}
     := {
      permut_mem :
        forall {A}, forall `{Inhabited A}, forall
        (s1 : sequence A)
        (s2 : sequence A),
        (permut s1 s2)
        ->
        (forall (x : A), (__belongs x s1) <-> (__belongs x s2))
    }.

    Class _permut_sub_sig `{@_sequence_sig}  := {
      permut_sub :
        forall {A}, forall `{Inhabited A}, (t A) ->
        (t A) ->
        integer ->
        integer ->
        prop
    }.

    Class _permut_sub_def_sig `{@_sequence_sig}
    `{@_permut_sig _}
    `{@___seq_sub_sig _}
    `{@___seq_sub_r_sig _}
    `{@___seq_sub_l_sig _}
    `{@_permut_sub_sig _}
     := {
      permut_sub_def :
        forall {A}, forall `{Inhabited A}, forall
        (s1 : sequence A)
        (s2 : sequence A)
        (i : integer)
        (j : integer),
        (permut (__seq_sub s1 i j) (__seq_sub s2 i j)) ->
        ((__seq_sub_r s1 i) = (__seq_sub_r s2 i)) ->
        ((__seq_sub_l s1 j) = (__seq_sub_l s2 j))
        ->
        (permut_sub s1 s2 i j)
    }.

  End Sequence.

  Module Bag.

    Definition t `{@_bag_sig} (A : Type) : Type := bag A.

    Class ___multiplicity_sig `{@_bag_sig}  := {
      __multiplicity :
        forall {A}, forall `{Inhabited A}, A -> (t A) -> integer
    }.

    Class _well_formed_sig `{@_bag_sig} `{@___multiplicity_sig _}  := {
      well_formed :
        forall {A}, forall `{Inhabited A}, forall (b : bag A) (x : A), (__multiplicity x b)
        >=
        0
    }.

    Class ___empty_sig `{@_bag_sig}  := {
      __empty :
        forall {A}, forall `{Inhabited A}, t A
    }.

    Class _empty_mult_sig `{@_bag_sig}
    `{@___multiplicity_sig _}
    `{@___empty_sig _}
     := {
      empty_mult :
        forall {A}, forall `{Inhabited A}, forall (x : A), (__multiplicity x __empty)
        =
        0
    }.

    Class _init_sig `{@_bag_sig}  := {
      init :
        forall {A}, forall `{Inhabited A}, (A -> integer) -> (t A)
    }.

    Class _init_axiom_sig `{@_max_sig}
    `{@_bag_sig}
    `{@___multiplicity_sig _}
    `{@_init_sig _}
     := {
      init_axiom :
        forall {A}, forall `{Inhabited A}, forall (f : A -> integer) (x : A), (max 0 (f x))
        =
        (__multiplicity x (init f))
    }.

    Class ___belongs_sig `{@_bag_sig}  := {
      __belongs :
        forall {A}, forall `{Inhabited A}, A -> (t A) -> prop
    }.

    Class _mem_sig `{@_bag_sig}  := {
      mem :
        forall {A}, forall `{Inhabited A}, A -> (t A) -> prop
    }.

    Class _mem_fun_def_sig `{@_bag_sig} `{@___belongs_sig _} `{@_mem_sig _}  := {
      mem_fun_def :
        forall {A}, forall `{Inhabited A}, forall (x : A) (s : bag A), (__belongs x s)
        <->
        (mem x s)
    }.

    Class ___neg_belongs_sig `{@_bag_sig}  := {
      __neg_belongs :
        forall {A}, forall `{Inhabited A}, A -> (t A) -> prop
    }.

    Class _nmem_def_sig `{@_bag_sig}
    `{@___neg_belongs_sig _}
    `{@___belongs_sig _}
     := {
      nmem_def :
        forall {A}, forall `{Inhabited A}, forall (s : bag A) (x : A), (__neg_belongs x s)
        <->
        (not (__belongs x s))
    }.

    Class _mem_def_sig `{@_bag_sig}
    `{@___belongs_sig _}
    `{@___multiplicity_sig _}
     := {
      mem_def :
        forall {A}, forall `{Inhabited A}, forall (x : A) (b : bag A), (__belongs x b)
        <->
        ((__multiplicity x b) > 0)
    }.

    Class _add_sig `{@_bag_sig}  := {
      add :
        forall {A}, forall `{Inhabited A}, A -> (t A) -> t A
    }.

    Class _add_mult_x_sig `{@_bag_sig}
    `{@___multiplicity_sig _}
    `{@_add_sig _}
     := {
      add_mult_x :
        forall {A}, forall `{Inhabited A}, forall (b : bag A) (x : A), (__multiplicity x (add x b))
        =
        (1 + (__multiplicity x b))
    }.

    Class _add_mult_neg_x_sig `{@_bag_sig}
    `{@___multiplicity_sig _}
    `{@_add_sig _}
     := {
      add_mult_neg_x :
        forall {A}, forall `{Inhabited A}, forall (x : A) (y : A) (b : bag A), (x
        <>
        y)
        ->
        ((__multiplicity y (add x b)) = (__multiplicity y b))
    }.

    Class ___singleton_set_sig `{@_bag_sig}  := {
      __singleton_set :
        forall {A}, forall `{Inhabited A}, A -> (t A)
    }.

    Class _singleton_sig `{@_bag_sig}  := {
      singleton :
        forall {A}, forall `{Inhabited A}, A -> (t A)
    }.

    Class _singleton_fun_def_sig `{@_bag_sig}
    `{@___singleton_set_sig _}
    `{@_singleton_sig _}
     := {
      singleton_fun_def :
        forall {A}, forall `{Inhabited A}, forall (x : A), (__singleton_set x)
        =
        (singleton x)
    }.

    Class _singleton_def_sig `{@_bag_sig}
    `{@___singleton_set_sig _}
    `{@_add_sig _}
    `{@___empty_sig _}
     := {
      singleton_def :
        forall {A}, forall `{Inhabited A}, forall (x : A), (__singleton_set x)
        =
        (add x __empty)
    }.

    Class _remove_sig `{@_bag_sig}  := {
      remove :
        forall {A}, forall `{Inhabited A}, A -> (t A) -> t A
    }.

    Class _remove_mult_x_sig `{@_bag_sig}
    `{@___multiplicity_sig _}
    `{@_remove_sig _}
    `{@_max_sig}
     := {
      remove_mult_x :
        forall {A}, forall `{Inhabited A}, forall (b : bag A) (x : A), (__multiplicity x (remove x b))
        =
        (max 0 ((__multiplicity x b) - 1))
    }.

    Class _remove_mult_neg_x_sig `{@_bag_sig}
    `{@___multiplicity_sig _}
    `{@_remove_sig _}
     := {
      remove_mult_neg_x :
        forall {A}, forall `{Inhabited A}, forall (x : A) (y : A) (b : bag A), (x
        <>
        y)
        ->
        ((__multiplicity y (remove x b)) = (__multiplicity y b))
    }.

    Class ___union_sig `{@_bag_sig}  := {
      __union :
        forall {A}, forall `{Inhabited A}, (t A) -> (t A) -> t A
    }.

    Class _union_all_sig `{@_bag_sig}
    `{@_max_sig}
    `{@___multiplicity_sig _}
    `{@___union_sig _}
     := {
      union_all :
        forall {A}, forall `{Inhabited A}, forall
        (b : bag A)
        (b' : bag A)
        (x : A),
        (max (__multiplicity x b) (__multiplicity x b'))
        =
        (__multiplicity x (__union b b'))
    }.

    Class _sum_sig `{@_bag_sig}  := {
      sum :
        forall {A}, forall `{Inhabited A}, (t A) -> (t A) -> t A
    }.

    Class _sum_all_sig `{@_bag_sig}
    `{@___multiplicity_sig _}
    `{@_sum_sig _}
     := {
      sum_all :
        forall {A}, forall `{Inhabited A}, forall
        (b : bag A)
        (b' : bag A)
        (x : A),
        ((__multiplicity x b) + (__multiplicity x b'))
        =
        (__multiplicity x (sum b b'))
    }.

    Class ___inter_sig `{@_bag_sig}  := {
      __inter :
        forall {A}, forall `{Inhabited A}, (t A) -> (t A) -> t A
    }.

    Class _inter_all_sig `{@_bag_sig}
    `{@_min_sig}
    `{@___multiplicity_sig _}
    `{@___inter_sig _}
     := {
      inter_all :
        forall {A}, forall `{Inhabited A}, forall
        (b : bag A)
        (b' : bag A)
        (x : A),
        (min (__multiplicity x b) (__multiplicity x b'))
        =
        (__multiplicity x (__inter b b'))
    }.

    Class _disjoint_sig `{@_bag_sig}  := {
      disjoint :
        forall {A}, forall `{Inhabited A}, (t A) -> (t A) -> prop
    }.

    Class _disjoint_def_sig `{@_bag_sig}
    `{@_disjoint_sig _}
    `{@___belongs_sig _}
    `{@___neg_belongs_sig _}
     := {
      disjoint_def :
        forall {A}, forall `{Inhabited A}, forall (b : bag A) (b' : bag A), (disjoint b b')
        <->
        (forall (x : A), (__belongs x b) -> (__neg_belongs x b'))
    }.

    Class _diff_sig `{@_bag_sig}  := {
      diff :
        forall {A}, forall `{Inhabited A}, (t A) -> (t A) -> t A
    }.

    Class _diff_all_sig `{@_bag_sig}
    `{@_max_sig}
    `{@___multiplicity_sig _}
    `{@_diff_sig _}
     := {
      diff_all :
        forall {A}, forall `{Inhabited A}, forall
        (b : bag A)
        (b' : bag A)
        (x : A),
        (max 0 ((__multiplicity x b) - (__multiplicity x b')))
        =
        (__multiplicity x (diff b b'))
    }.

    Class ___subset_sig `{@_bag_sig}  := {
      __subset :
        forall {A}, forall `{Inhabited A}, (t A) -> (t A) -> prop
    }.

    Class _subset_def_sig `{@_bag_sig}
    `{@___subset_sig _}
    `{@___multiplicity_sig _}
     := {
      subset_def :
        forall {A}, forall `{Inhabited A}, forall (b : bag A) (b' : bag A), (__subset b b')
        <->
        (forall (x : A), (__multiplicity x b) <= (__multiplicity x b'))
    }.

    Class _filter_sig `{@_bag_sig}  := {
      filter :
        forall {A}, forall `{Inhabited A}, (A -> prop) -> (t A) -> t A
    }.

    Class _filter_mem_sig `{@_bag_sig}
    `{@___multiplicity_sig _}
    `{@_filter_sig _}
     := {
      filter_mem :
        forall {A}, forall `{Inhabited A}, forall
        (b : bag A)
        (x : A)
        (f : A -> prop),
        (f x) -> ((__multiplicity x (filter f b)) = (__multiplicity x b))
    }.

    Class _filter_mem_neg_sig `{@_bag_sig}
    `{@___multiplicity_sig _}
    `{@_filter_sig _}
     := {
      filter_mem_neg :
        forall {A}, forall `{Inhabited A}, forall
        (b : bag A)
        (x : A)
        (f : A -> prop),
        (not (f x)) -> ((__multiplicity x (filter f b)) = 0)
    }.

    Class _cardinal_sig `{@_bag_sig}  := {
      cardinal :
        forall {A}, forall `{Inhabited A}, (t A) -> integer
    }.

    Class _finite_sig `{@_bag_sig}  := {
      finite :
        forall {A}, forall `{Inhabited A}, (t A) -> prop
    }.

    Class _finite_def_sig `{@_bag_sig}
    `{@_finite_sig _}
    `{@_sequence_sig}
    `{@___belongs_sig _}
    `{@Sequence.___belongs_sig _}
     := {
      finite_def :
        forall {A}, forall `{Inhabited A}, forall (b : bag A), (finite b)
        <->
        (exists (s : sequence A), forall (x : A), (__belongs x b)
        ->
        (Sequence.__belongs x s))
    }.

    Class _card_nonneg_sig `{@_bag_sig} `{@_cardinal_sig _}  := {
      card_nonneg :
        forall {A}, forall `{Inhabited A}, forall (b : bag A), (cardinal b) >= 0
    }.

    Class _card_empty_sig `{@_bag_sig}
    `{@_cardinal_sig _}
    `{@___empty_sig _}
     := {
      card_empty :
        forall {A}, forall `{Inhabited A}, (cardinal (A := A) __empty) = 0
    }.

    Class _card_singleton_sig `{@_bag_sig}
    `{@_cardinal_sig _}
    `{@___singleton_set_sig _}
     := {
      card_singleton :
        forall {A}, forall `{Inhabited A}, forall (x : A), (cardinal (__singleton_set x))
        =
        1
    }.

    Class _card_union_sig `{@_bag_sig}
    `{@_finite_sig _}
    `{@_cardinal_sig _}
    `{@___union_sig _}
     := {
      card_union :
        forall {A}, forall `{Inhabited A}, forall (b1 : bag A) (b2 : bag A), (finite b1) ->
        (finite b2) ->
        (cardinal (__union b1 b2))
        =
        ((cardinal b1) + (cardinal b2))
    }.

    Class _card_add_sig `{@_bag_sig}
    `{@_finite_sig _}
    `{@_cardinal_sig _}
    `{@_add_sig _}
     := {
      card_add :
        forall {A}, forall `{Inhabited A}, forall (x : A) (b : bag A), (finite b)
        ->
        ((cardinal (add x b)) = ((cardinal b) + 1))
    }.

    Class _card_map_sig `{@_bag_sig}
    `{@_finite_sig _}
    `{@_cardinal_sig _}
    `{@_filter_sig _}
     := {
      card_map :
        forall {A}, forall `{Inhabited A}, forall (f : A -> prop) (b : bag A), (finite b)
        ->
        ((cardinal (filter f b)) <= (cardinal b))
    }.

    Class _of_seq_sig `{@_bag_sig} `{@_sequence_sig}  := {
      of_seq :
        forall {A}, forall `{Inhabited A}, (Sequence.t A) -> (t A)
    }.

    Class _of_seq_multiplicity_sig `{@_sequence_sig}
    `{@Sequence.___multiplicity_sig _}
    `{@_bag_sig}
    `{@___multiplicity_sig _}
    `{@_of_seq_sig _ _}
     := {
      of_seq_multiplicity :
        forall {A}, forall `{Inhabited A}, forall (s : sequence A) (x : A), (Sequence.__multiplicity x s)
        =
        (__multiplicity x (of_seq s))
    }.

  End Bag.

  Module ___Set.

    Definition t `{@_set_sig} (A : Type) : Type := set A.

    Class ___empty_sig `{@_set_sig}  := {
      __empty :
        forall {A}, forall `{Inhabited A}, t A
    }.

    Class ___belongs_sig `{@_set_sig}  := {
      __belongs :
        forall {A}, forall `{Inhabited A}, A -> (t A) -> prop
    }.

    Class ___neg_belongs_sig `{@_set_sig}  := {
      __neg_belongs :
        forall {A}, forall `{Inhabited A}, A -> (t A) -> prop
    }.

    Class _mem_sig `{@_set_sig}  := {
      mem :
        forall {A}, forall `{Inhabited A}, A -> (t A) -> prop
    }.

    Class _mem_fun_def_sig `{@_set_sig} `{@___belongs_sig _} `{@_mem_sig _}  := {
      mem_fun_def :
        forall {A}, forall `{Inhabited A}, forall (x : A) (s : set A), (__belongs x s)
        <->
        (mem x s)
    }.

    Class _nmem_def_sig `{@_set_sig}
    `{@___belongs_sig _}
    `{@___neg_belongs_sig _}
     := {
      nmem_def :
        forall {A}, forall `{Inhabited A}, forall (s : set A) (x : A), (not (__belongs x s))
        <->
        (__neg_belongs x s)
    }.

    Class _empty_mem_sig `{@_set_sig}
    `{@___neg_belongs_sig _}
    `{@___empty_sig _}
     := {
      empty_mem :
        forall {A}, forall `{Inhabited A}, forall (x : A), __neg_belongs x __empty
    }.

    Class _add_sig `{@_set_sig}  := {
      add :
        forall {A}, forall `{Inhabited A}, A -> (t A) -> t A
    }.

    Class _add_mem_sig `{@_set_sig} `{@___belongs_sig _} `{@_add_sig _}  := {
      add_mem :
        forall {A}, forall `{Inhabited A}, forall (s : set A) (x : A), __belongs x (add x s)
    }.

    Class _add_mem_neq_sig `{@_set_sig} `{@___belongs_sig _} `{@_add_sig _}  := {
      add_mem_neq :
        forall {A}, forall `{Inhabited A}, forall (s : set A) (x : A) (y : A), (x
        <>
        y)
        ->
        ((__belongs x s) <-> (__belongs x (add y s)))
    }.

    Class ___singleton_set_sig `{@_set_sig}  := {
      __singleton_set :
        forall {A}, forall `{Inhabited A}, A -> (t A)
    }.

    Class _singleton_sig `{@_set_sig}  := {
      singleton :
        forall {A}, forall `{Inhabited A}, A -> (t A)
    }.

    Class _singleton_fun_def_sig `{@_set_sig}
    `{@___singleton_set_sig _}
    `{@_singleton_sig _}
     := {
      singleton_fun_def :
        forall {A}, forall `{Inhabited A}, forall (x : A), (__singleton_set x)
        =
        (singleton x)
    }.

    Class _singleton_def_sig `{@_set_sig}
    `{@___singleton_set_sig _}
    `{@_add_sig _}
    `{@___empty_sig _}
     := {
      singleton_def :
        forall {A}, forall `{Inhabited A}, forall (x : A), (__singleton_set x)
        =
        (add x __empty)
    }.

    Class _remove_sig `{@_set_sig}  := {
      remove :
        forall {A}, forall `{Inhabited A}, A -> (t A) -> t A
    }.

    Class _remove_mem_sig `{@_set_sig}
    `{@___neg_belongs_sig _}
    `{@_remove_sig _}
     := {
      remove_mem :
        forall {A}, forall `{Inhabited A}, forall (s : set A) (x : A), __neg_belongs x (remove x s)
    }.

    Class _remove_mem_neq_sig `{@_set_sig}
    `{@___belongs_sig _}
    `{@_remove_sig _}
     := {
      remove_mem_neq :
        forall {A}, forall `{Inhabited A}, forall (s : set A) (x : A) (y : A), (x
        <>
        y)
        ->
        ((__belongs x s) <-> (__belongs x (remove y s)))
    }.

    Class ___union_sig `{@_set_sig}  := {
      __union :
        forall {A}, forall `{Inhabited A}, (t A) -> (t A) -> t A
    }.

    Class _union_mem_sig `{@_set_sig}
    `{@___belongs_sig _}
    `{@___union_sig _}
     := {
      union_mem :
        forall {A}, forall `{Inhabited A}, forall
        (s : set A)
        (s' : set A)
        (x : A),
        ((__belongs x s) \/ (__belongs x s')) -> (__belongs x (__union s s'))
    }.

    Class _union_mem_neg_sig `{@_set_sig}
    `{@___neg_belongs_sig _}
    `{@___union_sig _}
     := {
      union_mem_neg :
        forall {A}, forall `{Inhabited A}, forall
        (s : set A)
        (s' : set A)
        (x : A),
        (__neg_belongs x s) ->
        (__neg_belongs x s') ->
        __neg_belongs x (__union s s')
    }.

    Class ___inter_sig `{@_set_sig}  := {
      __inter :
        forall {A}, forall `{Inhabited A}, (t A) -> (t A) -> t A
    }.

    Class _inter_mem_sig `{@_set_sig}
    `{@___belongs_sig _}
    `{@___inter_sig _}
     := {
      inter_mem :
        forall {A}, forall `{Inhabited A}, forall
        (s : set A)
        (s' : set A)
        (x : A),
        (__belongs x s) -> (__belongs x s') -> __belongs x (__inter s s')
    }.

    Class _inter_mem_neq_sig `{@_set_sig}
    `{@___belongs_sig _}
    `{@___neg_belongs_sig _}
    `{@___inter_sig _}
     := {
      inter_mem_neq :
        forall {A}, forall `{Inhabited A}, forall
        (s : set A)
        (s' : set A)
        (x : A),
        (not ((__belongs x s) \/ (__belongs x s')))
        ->
        (__neg_belongs x (__inter s s'))
    }.

    Class _disjoint_sig `{@_set_sig}  := {
      disjoint :
        forall {A}, forall `{Inhabited A}, (t A) -> (t A) -> prop
    }.

    Class _disjoint_def_sig `{@_set_sig}
    `{@_disjoint_sig _}
    `{@___inter_sig _}
    `{@___empty_sig _}
     := {
      disjoint_def :
        forall {A}, forall `{Inhabited A}, forall (s : set A) (s' : set A), (disjoint s s')
        <->
        ((__inter s s') = __empty)
    }.

    Class ___diff_sig `{@_set_sig}  := {
      __diff :
        forall {A}, forall `{Inhabited A}, (t A) -> (t A) -> t A
    }.

    Class _diff_mem_sig `{@_set_sig}
    `{@___belongs_sig _}
    `{@___neg_belongs_sig _}
    `{@___diff_sig _}
     := {
      diff_mem :
        forall {A}, forall `{Inhabited A}, forall
        (s : set A)
        (s' : set A)
        (x : A),
        (__belongs x s') -> (__neg_belongs x (__diff s s'))
    }.

    Class _diff_mem_fst_sig `{@_set_sig}
    `{@___neg_belongs_sig _}
    `{@___belongs_sig _}
    `{@___diff_sig _}
     := {
      diff_mem_fst :
        forall {A}, forall `{Inhabited A}, forall
        (s : set A)
        (s' : set A)
        (x : A),
        (__neg_belongs x s')
        ->
        ((__belongs x s) <-> (__belongs x (__diff s s')))
    }.

    Class ___subset_sig `{@_set_sig}  := {
      __subset :
        forall {A}, forall `{Inhabited A}, (t A) -> (t A) -> prop
    }.

    Class _subset_def_sig `{@_set_sig}
    `{@___subset_sig _}
    `{@___belongs_sig _}
     := {
      subset_def :
        forall {A}, forall `{Inhabited A}, forall (s : set A) (s' : set A), (__subset s s')
        <->
        (forall (x : A), (__belongs x s) -> (__belongs x s'))
    }.

    Class _map_sig `{@_set_sig}  := {
      map :
        forall {A} {B}, forall `{Inhabited A} `{Inhabited B}, (A -> B) ->
        (t A) ->
        t B
    }.

    Class _set_map_sig `{@_set_sig} `{@___belongs_sig _} `{@_map_sig _}  := {
      set_map :
        forall {A} {B}, forall `{Inhabited A} `{Inhabited B}, forall
        (f : B -> A)
        (s : set B)
        (x : A),
        (__belongs x (map f s))
        <->
        (exists (y : B), ((f y) = x) /\ (__belongs y s))
    }.

    Class _partition_sig `{@_set_sig}  := {
      partition :
        forall {A}, forall `{Inhabited A}, (A -> prop) -> (t A) -> (t A) * (t A)
    }.

    Class _partition_l_mem_sig `{@_set_sig}
    `{@___belongs_sig _}
    `{@_partition_sig _}
     := {
      partition_l_mem :
        forall {A}, forall `{Inhabited A}, forall
        (f : A -> prop)
        (s : set A)
        (x : A)
        (p1 : set A)
        (p2 : set A),
        (__belongs x s) ->
        (f x) ->
        ((partition f s) = ( p1, p2 ))
        ->
        (__belongs x p1)
    }.

    Class _partition_r_mem_sig `{@_set_sig}
    `{@___belongs_sig _}
    `{@_partition_sig _}
     := {
      partition_r_mem :
        forall {A}, forall `{Inhabited A}, forall
        (f : A -> prop)
        (s : set A)
        (x : A)
        (p1 : set A)
        (p2 : set A),
        (__belongs x s) ->
        (not (f x)) ->
        ((partition f s) = ( p1, p2 ))
        ->
        (__belongs x p2)
    }.

    Class _cardinal_sig `{@_set_sig}  := {
      cardinal :
        forall {A}, forall `{Inhabited A}, (t A) -> integer
    }.

    Class _finite_sig `{@_set_sig}  := {
      finite :
        forall {A}, forall `{Inhabited A}, (t A) -> prop
    }.

    Class _finite_def_sig `{@_set_sig}
    `{@_finite_sig _}
    `{@_sequence_sig}
    `{@___belongs_sig _}
    `{@Sequence.___belongs_sig _}
     := {
      finite_def :
        forall {A}, forall `{Inhabited A}, forall (s : set A), (finite s)
        <->
        (exists (seq : sequence A), forall (x : A), (__belongs x s)
        ->
        (Sequence.__belongs x seq))
    }.

    Class _cardinal_nonneg_sig `{@_set_sig} `{@_cardinal_sig _}  := {
      cardinal_nonneg :
        forall {A}, forall `{Inhabited A}, forall (s : set A), (cardinal s) >= 0
    }.

    Class _cardinal_empty_sig `{@_set_sig}
    `{@_cardinal_sig _}
    `{@___empty_sig _}
     := {
      cardinal_empty :
        forall {A}, forall `{Inhabited A}, (cardinal (A := A) __empty) = 0
    }.

    Class _cardinal_remove_mem_sig `{@_set_sig}
    `{@_finite_sig _}
    `{@___belongs_sig _}
    `{@_cardinal_sig _}
    `{@_remove_sig _}
     := {
      cardinal_remove_mem :
        forall {A}, forall `{Inhabited A}, forall (s : set A) (x : A), (finite s) ->
        (__belongs x s) ->
        (cardinal (remove x s))
        =
        ((cardinal s) - 1)
    }.

    Class _cardinal_remove_not_mem_sig `{@_set_sig}
    `{@_finite_sig _}
    `{@___neg_belongs_sig _}
    `{@_cardinal_sig _}
    `{@_remove_sig _}
     := {
      cardinal_remove_not_mem :
        forall {A}, forall `{Inhabited A}, forall (s : set A) (x : A), (finite s) ->
        (__neg_belongs x s) ->
        (cardinal (remove x s))
        =
        (cardinal s)
    }.

    Class _cardinal_add_sig `{@_set_sig}
    `{@_finite_sig _}
    `{@___neg_belongs_sig _}
    `{@_cardinal_sig _}
    `{@_add_sig _}
     := {
      cardinal_add :
        forall {A}, forall `{Inhabited A}, forall (s : set A) (x : A), (finite s) ->
        (__neg_belongs x s) ->
        (cardinal (add x s))
        =
        ((cardinal s) + 1)
    }.

    Class _cardinal_add_mem_sig `{@_set_sig}
    `{@_finite_sig _}
    `{@___belongs_sig _}
    `{@_cardinal_sig _}
    `{@_add_sig _}
     := {
      cardinal_add_mem :
        forall {A}, forall `{Inhabited A}, forall (s : set A) (x : A), (finite s) ->
        (__belongs x s) ->
        (cardinal (add x s))
        =
        (cardinal s)
    }.

    Class _of_seq_sig `{@_set_sig} `{@_sequence_sig}  := {
      of_seq :
        forall {A}, forall `{Inhabited A}, (Sequence.t A) -> (t A)
    }.

    Class _of_seq_mem_sig `{@_sequence_sig}
    `{@_set_sig}
    `{@___belongs_sig _}
    `{@_of_seq_sig _ _}
    `{@Sequence.___belongs_sig _}
     := {
      of_seq_mem :
        forall {A}, forall `{Inhabited A}, forall (s : sequence A), forall
        (x : A),
        (__belongs x (of_seq s)) <-> (Sequence.__belongs x s)
    }.

    Class _to_seq_sig `{@_sequence_sig} `{@_set_sig}  := {
      to_seq :
        forall {A}, forall `{Inhabited A}, (t A) -> (Sequence.t A)
    }.

    Class _to_seq_mem_sig `{@_set_sig}
    `{@_finite_sig _}
    `{@___belongs_sig _}
    `{@_sequence_sig}
    `{@Sequence.___multiplicity_sig _}
    `{@_to_seq_sig _ _}
     := {
      to_seq_mem :
        forall {A}, forall `{Inhabited A}, forall (s : set A), (finite s)
        ->
        (forall (x : A), (__belongs x s)
        <->
        ((Sequence.__multiplicity x (to_seq s)) = 1))
    }.

    Class _fold_sig `{@_set_sig}  := {
      fold :
        forall {A} {B}, forall `{Inhabited A} `{Inhabited B}, (A -> B) ->
        (B -> B -> B) ->
        (t A) ->
        B ->
        B
    }.

    Class _fold_def_sig `{@_set_sig}
    `{@_finite_sig _}
    `{@_comm_monoid_sig}
    `{@_fold_sig _}
    `{@_sequence_sig}
    `{@Sequence._fold_right_sig _}
    `{@_to_seq_sig _ _}
     := {
      fold_def :
        forall {A} {B}, forall `{Inhabited A} `{Inhabited B}, forall
        (f : A -> B),
        forall (m : B -> B -> B) (s : set A) (acc : B), (finite s) ->
        (comm_monoid m acc) ->
        (fold f m s acc)
        =
        (Sequence.fold_right (fun (x : A) => fun (acc : B) => m (f x) acc) (to_seq s) acc)
    }.

  End ___Set.

  Module Fin_maps.

    Definition t `{@_fin_map_sig} (K : Type) (V : Type) : Type :=
      fin_map K V.

    Class ___empty_sig `{@_fin_map_sig}  := {
      __empty :
        forall {V} {K}, forall `{Inhabited V} `{Inhabited K}, t K V
    }.

    Class _singleton_sig `{@_fin_map_sig}  := {
      singleton :
        forall {V} {K}, forall `{Inhabited V} `{Inhabited K}, K -> V -> t K V
    }.

    Class _get_sig `{@_fin_map_sig}  := {
      get :
        forall {V} {K}, forall `{Inhabited V} `{Inhabited K}, (t K V) -> K -> V
    }.

    Class ___seq_get_sig `{@_fin_map_sig}  := {
      __seq_get :
        forall {V} {K}, forall `{Inhabited V} `{Inhabited K}, (t K V) -> K -> V
    }.

    Class _get_fun_def_sig `{@_fin_map_sig}
    `{@_get_sig _}
    `{@___seq_get_sig _}
     := {
      get_fun_def :
        forall {B} {A}, forall `{Inhabited B} `{Inhabited A}, forall
        (m : fin_map A B)
        (k : A),
        (get m k) = (__seq_get m k)
    }.

    Class _add_sig `{@_fin_map_sig}  := {
      add :
        forall {V} {K}, forall `{Inhabited V} `{Inhabited K}, K ->
        V ->
        (t K V)
        ->
        (t K V)
    }.

    Class _singleton_def_sig `{@_fin_map_sig}
    `{@_singleton_sig _}
    `{@_add_sig _}
    `{@___empty_sig _}
     := {
      singleton_def :
        forall {B} {A}, forall `{Inhabited B} `{Inhabited A}, forall
        (k : A)
        (v : B),
        (singleton k v) = (add k v __empty)
    }.

    Class _add_get_sig `{@_fin_map_sig} `{@___seq_get_sig _} `{@_add_sig _}  := {
      add_get :
        forall {B} {A}, forall `{Inhabited B} `{Inhabited A}, forall
        (k : A)
        (v : B)
        (m : fin_map A B),
        (__seq_get (add k v m) k) = v
    }.

    Class _add_get_neq_sig `{@_fin_map_sig}
    `{@___seq_get_sig _}
    `{@_add_sig _}
     := {
      add_get_neq :
        forall {A} {B}, forall `{Inhabited A} `{Inhabited B}, forall
        (k : A)
        (k' : A)
        (v : B)
        (m : fin_map A B),
        (k <> k') -> ((__seq_get (add k v m) k') = (__seq_get m k'))
    }.

    Class _mem_sig `{@_fin_map_sig}  := {
      mem :
        forall {V} {K}, forall `{Inhabited V} `{Inhabited K}, (t K V) ->
        K ->
        V
        ->
        prop
    }.

    Class ___belongs_sig `{@_fin_map_sig}  := {
      __belongs :
        forall {V} {K}, forall `{Inhabited V} `{Inhabited K}, (K * V) ->
        (t K V) ->
        prop
    }.

    Class _mem_fun_def_sig `{@_fin_map_sig}
    `{@_mem_sig _}
    `{@___belongs_sig _}
     := {
      mem_fun_def :
        forall {A} {B}, forall `{Inhabited A} `{Inhabited B}, forall
        (k : A)
        (v : B)
        (m : fin_map A B),
        (mem m k v) <-> (__belongs ( k, v ) m)
    }.

    Class _mem_get_sig `{@_fin_map_sig}
    `{@___belongs_sig _}
    `{@___seq_get_sig _}
     := {
      mem_get :
        forall {A} {B}, forall `{Inhabited A} `{Inhabited B}, forall
        (k : A)
        (v : B)
        (m : fin_map A B),
        (__belongs ( k, v ) m) -> ((__seq_get m k) = v)
    }.

    Class ___neg_belongs_sig `{@_fin_map_sig}  := {
      __neg_belongs :
        forall {V} {K}, forall `{Inhabited V} `{Inhabited K}, (K * V) ->
        (t K V) ->
        prop
    }.

    Class _nmem_def_sig `{@_fin_map_sig}
    `{@___neg_belongs_sig _}
    `{@___belongs_sig _}
     := {
      nmem_def :
        forall {B} {A}, forall `{Inhabited B} `{Inhabited A}, forall
        (k : A)
        (v : B)
        (m : fin_map A B),
        (__neg_belongs ( k, v ) m) <-> (not (__belongs ( k, v ) m))
    }.

    Class _mem_empty_sig `{@_fin_map_sig}
    `{@___neg_belongs_sig _}
    `{@___empty_sig _}
     := {
      mem_empty :
        forall {B} {A}, forall `{Inhabited B} `{Inhabited A}, forall
        (k : A)
        (v : B),
        __neg_belongs ( k, v ) __empty
    }.

    Class _nmem_add_sig `{@_fin_map_sig}
    `{@___neg_belongs_sig _}
    `{@_add_sig _}
     := {
      nmem_add :
        forall {B} {A}, forall `{Inhabited B} `{Inhabited A}, forall
        (k : A)
        (v : B)
        (k' : A)
        (v' : B)
        (m : fin_map A B),
        (k' <> k)
        ->
        ((__neg_belongs ( k', v' ) m) <-> (__neg_belongs ( k', v' ) (add k v m)))
    }.

    Class _remove_sig `{@_fin_map_sig}  := {
      remove :
        forall {V} {K}, forall `{Inhabited V} `{Inhabited K}, K ->
        (t K V) ->
        t K V
    }.

    Class _mem_remove_sig `{@_fin_map_sig}
    `{@___neg_belongs_sig _}
    `{@_remove_sig _}
     := {
      mem_remove :
        forall {A} {C} {B}, forall
        `{Inhabited A}
        `{Inhabited C}
        `{Inhabited B},
        forall (k : A) (m : fin_map A B) (m' : C) (v : B), __neg_belongs ( k, v ) (remove k m)
    }.

    Class _nmem_remove_sig `{@_fin_map_sig}
    `{@___neg_belongs_sig _}
    `{@_remove_sig _}
     := {
      nmem_remove :
        forall {A} {B}, forall `{Inhabited A} `{Inhabited B}, forall
        (k : A)
        (k' : A)
        (v : B)
        (m : fin_map A B),
        (k <> k')
        ->
        ((__neg_belongs ( k', v ) m) <-> (__neg_belongs ( k', v ) (remove k m)))
    }.

    Class _fmap_extensionality_sig `{@_fin_map_sig} `{@___belongs_sig _}  := {
      fmap_extensionality :
        forall {B} {A}, forall `{Inhabited B} `{Inhabited A}, forall
        (m : fin_map A B)
        (m' : fin_map A B),
        (forall (k : A) (v : B), (__belongs ( k, v ) m)
        <->
        (__belongs ( k, v ) m'))
        ->
        (m = m')
    }.

    Class _map_sig `{@_fin_map_sig}  := {
      map :
        forall {A} {V} {K}, forall
        `{Inhabited A}
        `{Inhabited V}
        `{Inhabited K},
        (V -> A) -> (t K V) -> t K A
    }.

    Class _map_mem_sig `{@_fin_map_sig} `{@___belongs_sig _} `{@_map_sig _}  := {
      map_mem :
        forall {B} {A} {C}, forall
        `{Inhabited B}
        `{Inhabited A}
        `{Inhabited C},
        forall (k : A) (f : C -> B) (v : C) (m : fin_map A C), (__belongs (
        k,
        v
        ) m)
        <->
        (__belongs ( k, f v ) (map f m))
    }.

    Class _to_seq_sig `{@_sequence_sig} `{@_fin_map_sig}  := {
      to_seq :
        forall {V} {K}, forall `{Inhabited V} `{Inhabited K}, (t K V)
        ->
        (Sequence.t (K * V))
    }.

    Class _to_seq_mem_sig `{@_fin_map_sig}
    `{@___belongs_sig _}
    `{@_sequence_sig}
    `{@Sequence.___multiplicity_sig _}
    `{@_to_seq_sig _ _}
     := {
      to_seq_mem :
        forall {B} {A}, forall `{Inhabited B} `{Inhabited A}, forall
        (k : A)
        (v : B)
        (m : fin_map A B),
        (__belongs ( k, v ) m)
        <->
        ((Sequence.__multiplicity ( k, v ) (to_seq m)) = 1)
    }.

    Class _population_sig `{@_fin_map_sig}  := {
      population :
        forall {V} {K}, forall `{Inhabited V} `{Inhabited K}, (t K V) -> integer
    }.

    Class _population_to_seq_sig `{@_fin_map_sig}
    `{@_population_sig _}
    `{@_sequence_sig}
    `{@Sequence._length_sig _}
    `{@_to_seq_sig _ _}
     := {
      population_to_seq :
        forall {A} {B}, forall `{Inhabited A} `{Inhabited B}, forall
        (m : fin_map A B),
        (population m) = (Sequence.length (to_seq m))
    }.

  End Fin_maps.

  Class ___map_set_sig  := {
    __map_set :
      forall {A} {B}, forall `{Inhabited A} `{Inhabited B}, (A -> B) ->
      A ->
      B ->
      A ->
      B
  }.

  Class _map_set_def_sig `{@___map_set_sig}  := {
    map_set_def :
      forall {A} {B}, forall `{Inhabited A} `{Inhabited B}, forall
      (f : A -> B),
      forall (x : A), forall (y : B), (__map_set f x y x) = y
  }.

  Class _map_set_def_neq_sig `{@___map_set_sig}  := {
    map_set_def_neq :
      forall {A} {B}, forall `{Inhabited A} `{Inhabited B}, forall
      (f : A -> B),
      forall (x : A), forall (y : B), forall (z : A), (x <> z)
      ->
      ((__map_set f x y z) = (f z))
  }.

  Module Map.

    Definition t (A : Type) (B : Type) : Type := map A B.

    Class _domain_sig `{@_set_sig}  := {
      domain :
        forall {A} {B}, forall `{Inhabited A} `{Inhabited B}, B ->
        (t A B) ->
        ___Set.t A
    }.

    Class _domain_mem_sig `{@_set_sig}
    `{@___Set.___belongs_sig _}
    `{@_domain_sig _}
     := {
      domain_mem :
        forall {B} {A}, forall `{Inhabited B} `{Inhabited A}, forall
        (x : A)
        (m : A -> B)
        (default : B),
        ((m x) <> default) -> (___Set.__belongs x (domain default m))
    }.

  End Map.

End Declarations.

Module Type Obligations.

  Import Declarations.

  Global Declare Instance _set_inst : _set_sig.

  (* Type definition sequence *)

  Global Declare Instance _sequence_inst : _sequence_sig.

  (* Type definition bag *)

  Global Declare Instance _bag_inst : _bag_sig.

  (* Type definition option *)

  Global Declare Instance _option_inst : _option_sig.

  (* Type definition fin_map *)

  Global Declare Instance _fin_map_inst : _fin_map_sig.

  (* Logical function Some *)

  Global Declare Instance _Some_inst : _Some_sig.

  (* Logical function None *)

  Global Declare Instance _None_inst : _None_sig.

  (* Logical function succ *)

  Global Declare Instance _succ_inst : _succ_sig.

  (* Logical function pred *)

  Global Declare Instance _pred_inst : _pred_sig.

  (* Logical function ___mod *)

  Global Declare Instance ____mod_inst : ____mod_sig.

  (* Logical function pow *)

  Global Declare Instance _pow_inst : _pow_sig.

  (* Logical function abs *)

  Global Declare Instance _abs_inst : _abs_sig.

  (* Logical function min *)

  Global Declare Instance _min_inst : _min_sig.

  (* Logical function max *)

  Global Declare Instance _max_inst : _max_sig.

  (* Logical function __app *)

  Global Declare Instance ___app_inst : ___app_sig.

  (* Logical function __seq_get *)

  Global Declare Instance ___seq_get_inst : ___seq_get_sig.

  (* Logical function __seq_sub *)

  Global Declare Instance ___seq_sub_inst : ___seq_sub_sig.

  (* Logical function __seq_sub_l *)

  Global Declare Instance ___seq_sub_l_inst : ___seq_sub_l_sig.

  (* Logical function __seq_sub_r *)

  Global Declare Instance ___seq_sub_r_inst : ___seq_sub_r_sig.

  (* Logical function monoid *)

  Global Declare Instance _monoid_inst : _monoid_sig.

  (* Axiom monoid_def *)

  Global Declare Instance _monoid_def_inst : _monoid_def_sig.

  (* Logical function comm_monoid *)

  Global Declare Instance _comm_monoid_inst : _comm_monoid_sig.

  (* Axiom comm_monoid_def *)

  Global Declare Instance _comm_monoid_def_inst : _comm_monoid_def_sig.

  Module Sequence.

    Import Declarations.Sequence.

    (* Logical function length *)

    Global Declare Instance _length_inst : _length_sig.

    (* Logical function in_range *)

    Global Declare Instance _in_range_inst : _in_range_sig.

    (* Axiom in_range_def *)

    Global Declare Instance _in_range_def_inst : _in_range_def_sig.

    (* Axiom length_nonneg *)

    Global Declare Instance _length_nonneg_inst : _length_nonneg_sig.

    (* Axiom subseq_l *)

    Global Declare Instance _subseq_l_inst : _subseq_l_sig.

    (* Axiom subseq_r *)

    Global Declare Instance _subseq_r_inst : _subseq_r_sig.

    (* Axiom subseq *)

    Global Declare Instance _subseq_inst : _subseq_sig.

    (* Axiom subseq_len *)

    Global Declare Instance _subseq_len_inst : _subseq_len_sig.

    (* Logical function empty *)

    Global Declare Instance _empty_inst : _empty_sig.

    (* Logical function __empty *)

    Global Declare Instance ___empty_inst : ___empty_sig.

    (* Axiom empty_length *)

    Global Declare Instance _empty_length_inst : _empty_length_sig.

    (* Logical function init *)

    Global Declare Instance _init_inst : _init_sig.

    (* Axiom init_length *)

    Global Declare Instance _init_length_inst : _init_length_sig.

    (* Axiom init_elems *)

    Global Declare Instance _init_elems_inst : _init_elems_sig.

    (* Logical function singleton *)

    Global Declare Instance _singleton_inst : _singleton_sig.

    (* Axiom singleton_def *)

    Global Declare Instance _singleton_def_inst : _singleton_def_sig.

    (* Logical function cons *)

    Global Declare Instance _cons_inst : _cons_sig.

    (* Axiom cons_def *)

    Global Declare Instance _cons_def_inst : _cons_def_sig.

    (* Logical function snoc *)

    Global Declare Instance _snoc_inst : _snoc_sig.

    (* Axiom snoc_def *)

    Global Declare Instance _snoc_def_inst : _snoc_def_sig.

    (* Logical function hd *)

    Global Declare Instance _hd_inst : _hd_sig.

    (* Axiom hd_def *)

    Global Declare Instance _hd_def_inst : _hd_def_sig.

    (* Logical function tl *)

    Global Declare Instance _tl_inst : _tl_sig.

    (* Axiom tl_def *)

    Global Declare Instance _tl_def_inst : _tl_def_sig.

    (* Logical function append *)

    Global Declare Instance _append_inst : _append_sig.

    (* Axiom append_def *)

    Global Declare Instance _append_def_inst : _append_def_sig.

    (* Axiom append_length *)

    Global Declare Instance _append_length_inst : _append_length_sig.

    (* Axiom append_elems_left *)

    Global Declare Instance _append_elems_left_inst : _append_elems_left_sig.

    (* Axiom append_elems_right *)

    Global Declare Instance _append_elems_right_inst : _append_elems_right_sig.

    (* Logical function __multiplicity *)

    Global Declare Instance ___multiplicity_inst : ___multiplicity_sig.

    (* Axiom mult_empty *)

    Global Declare Instance _mult_empty_inst : _mult_empty_sig.

    (* Axiom mult_cons *)

    Global Declare Instance _mult_cons_inst : _mult_cons_sig.

    (* Axiom mult_cons_neutral *)

    Global Declare Instance _mult_cons_neutral_inst : _mult_cons_neutral_sig.

    (* Axiom mult_length *)

    Global Declare Instance _mult_length_inst : _mult_length_sig.

    (* Logical function __belongs *)

    Global Declare Instance ___belongs_inst : ___belongs_sig.

    (* Logical function mem *)

    Global Declare Instance _mem_inst : _mem_sig.

    (* Axiom mem_fun_def *)

    Global Declare Instance _mem_fun_def_inst : _mem_fun_def_sig.

    (* Axiom mem_def *)

    Global Declare Instance _mem_def_inst : _mem_def_sig.

    (* Logical function __neg_belongs *)

    Global Declare Instance ___neg_belongs_inst : ___neg_belongs_sig.

    (* Axiom nmem_def *)

    Global Declare Instance _nmem_def_inst : _nmem_def_sig.

    (* Logical function Forall *)

    Global Declare Instance _Forall_inst : _Forall_sig.

    (* Axiom forall_def *)

    Global Declare Instance _forall_def_inst : _forall_def_sig.

    (* Logical function Exists *)

    Global Declare Instance _Exists_inst : _Exists_sig.

    (* Axiom exists_def *)

    Global Declare Instance _exists_def_inst : _exists_def_sig.

    (* Logical function map *)

    Global Declare Instance _map_inst : _map_sig.

    (* Axiom map_elems *)

    Global Declare Instance _map_elems_inst : _map_elems_sig.

    (* Axiom map_length *)

    Global Declare Instance _map_length_inst : _map_length_sig.

    (* Logical function filter *)

    Global Declare Instance _filter_inst : _filter_sig.

    (* Axiom filter_elems *)

    Global Declare Instance _filter_elems_inst : _filter_elems_sig.

    (* Logical function filter_map *)

    Global Declare Instance _filter_map_inst : _filter_map_sig.

    (* Axiom filter_map_elems *)

    Global Declare Instance _filter_map_elems_inst : _filter_map_elems_sig.

    (* Logical function get *)

    Global Declare Instance _get_inst : _get_sig.

    (* Axiom get_def *)

    Global Declare Instance _get_def_inst : _get_def_sig.

    (* Logical function set *)

    Global Declare Instance _set_inst : _set_sig.

    (* Axiom set_elem *)

    Global Declare Instance _set_elem_inst : _set_elem_sig.

    (* Axiom set_elem_other *)

    Global Declare Instance _set_elem_other_inst : _set_elem_other_sig.

    (* Logical function rev *)

    Global Declare Instance _rev_inst : _rev_sig.

    (* Axiom rev_length *)

    Global Declare Instance _rev_length_inst : _rev_length_sig.

    (* Axiom rev_elems *)

    Global Declare Instance _rev_elems_inst : _rev_elems_sig.

    (* Axiom extensionality *)

    Global Declare Instance _extensionality_inst : _extensionality_sig.

    (* Logical function fold_left *)

    Global Declare Instance _fold_left_inst : _fold_left_sig.

    (* Axiom fold_left_empty *)

    Global Declare Instance _fold_left_empty_inst : _fold_left_empty_sig.

    (* Axiom fold_left_cons *)

    Global Declare Instance _fold_left_cons_inst : _fold_left_cons_sig.

    (* Logical function fold_right *)

    Global Declare Instance _fold_right_inst : _fold_right_sig.

    (* Axiom fold_right_empty *)

    Global Declare Instance _fold_right_empty_inst : _fold_right_empty_sig.

    (* Axiom fold_right_cons *)

    Global Declare Instance _fold_right_cons_inst : _fold_right_cons_sig.

    (* Logical function permut *)

    Global Declare Instance _permut_inst : _permut_sig.

    (* Axiom permut_mem *)

    Global Declare Instance _permut_mem_inst : _permut_mem_sig.

    (* Logical function permut_sub *)

    Global Declare Instance _permut_sub_inst : _permut_sub_sig.

    (* Axiom permut_sub_def *)

    Global Declare Instance _permut_sub_def_inst : _permut_sub_def_sig.

  End Sequence.

  Module Bag.

    Import Declarations.Bag.

    (* Logical function __multiplicity *)

    Global Declare Instance ___multiplicity_inst : ___multiplicity_sig.

    (* Axiom well_formed *)

    Global Declare Instance _well_formed_inst : _well_formed_sig.

    (* Logical function __empty *)

    Global Declare Instance ___empty_inst : ___empty_sig.

    (* Axiom empty_mult *)

    Global Declare Instance _empty_mult_inst : _empty_mult_sig.

    (* Logical function init *)

    Global Declare Instance _init_inst : _init_sig.

    (* Axiom init_axiom *)

    Global Declare Instance _init_axiom_inst : _init_axiom_sig.

    (* Logical function __belongs *)

    Global Declare Instance ___belongs_inst : ___belongs_sig.

    (* Logical function mem *)

    Global Declare Instance _mem_inst : _mem_sig.

    (* Axiom mem_fun_def *)

    Global Declare Instance _mem_fun_def_inst : _mem_fun_def_sig.

    (* Logical function __neg_belongs *)

    Global Declare Instance ___neg_belongs_inst : ___neg_belongs_sig.

    (* Axiom nmem_def *)

    Global Declare Instance _nmem_def_inst : _nmem_def_sig.

    (* Axiom mem_def *)

    Global Declare Instance _mem_def_inst : _mem_def_sig.

    (* Logical function add *)

    Global Declare Instance _add_inst : _add_sig.

    (* Axiom add_mult_x *)

    Global Declare Instance _add_mult_x_inst : _add_mult_x_sig.

    (* Axiom add_mult_neg_x *)

    Global Declare Instance _add_mult_neg_x_inst : _add_mult_neg_x_sig.

    (* Logical function __singleton_set *)

    Global Declare Instance ___singleton_set_inst : ___singleton_set_sig.

    (* Logical function singleton *)

    Global Declare Instance _singleton_inst : _singleton_sig.

    (* Axiom singleton_fun_def *)

    Global Declare Instance _singleton_fun_def_inst : _singleton_fun_def_sig.

    (* Axiom singleton_def *)

    Global Declare Instance _singleton_def_inst : _singleton_def_sig.

    (* Logical function remove *)

    Global Declare Instance _remove_inst : _remove_sig.

    (* Axiom remove_mult_x *)

    Global Declare Instance _remove_mult_x_inst : _remove_mult_x_sig.

    (* Axiom remove_mult_neg_x *)

    Global Declare Instance _remove_mult_neg_x_inst : _remove_mult_neg_x_sig.

    (* Logical function __union *)

    Global Declare Instance ___union_inst : ___union_sig.

    (* Axiom union_all *)

    Global Declare Instance _union_all_inst : _union_all_sig.

    (* Logical function sum *)

    Global Declare Instance _sum_inst : _sum_sig.

    (* Axiom sum_all *)

    Global Declare Instance _sum_all_inst : _sum_all_sig.

    (* Logical function __inter *)

    Global Declare Instance ___inter_inst : ___inter_sig.

    (* Axiom inter_all *)

    Global Declare Instance _inter_all_inst : _inter_all_sig.

    (* Logical function disjoint *)

    Global Declare Instance _disjoint_inst : _disjoint_sig.

    (* Axiom disjoint_def *)

    Global Declare Instance _disjoint_def_inst : _disjoint_def_sig.

    (* Logical function diff *)

    Global Declare Instance _diff_inst : _diff_sig.

    (* Axiom diff_all *)

    Global Declare Instance _diff_all_inst : _diff_all_sig.

    (* Logical function __subset *)

    Global Declare Instance ___subset_inst : ___subset_sig.

    (* Axiom subset_def *)

    Global Declare Instance _subset_def_inst : _subset_def_sig.

    (* Logical function filter *)

    Global Declare Instance _filter_inst : _filter_sig.

    (* Axiom filter_mem *)

    Global Declare Instance _filter_mem_inst : _filter_mem_sig.

    (* Axiom filter_mem_neg *)

    Global Declare Instance _filter_mem_neg_inst : _filter_mem_neg_sig.

    (* Logical function cardinal *)

    Global Declare Instance _cardinal_inst : _cardinal_sig.

    (* Logical function finite *)

    Global Declare Instance _finite_inst : _finite_sig.

    (* Axiom finite_def *)

    Global Declare Instance _finite_def_inst : _finite_def_sig.

    (* Axiom card_nonneg *)

    Global Declare Instance _card_nonneg_inst : _card_nonneg_sig.

    (* Axiom card_empty *)

    Global Declare Instance _card_empty_inst : _card_empty_sig.

    (* Axiom card_singleton *)

    Global Declare Instance _card_singleton_inst : _card_singleton_sig.

    (* Axiom card_union *)

    Global Declare Instance _card_union_inst : _card_union_sig.

    (* Axiom card_add *)

    Global Declare Instance _card_add_inst : _card_add_sig.

    (* Axiom card_map *)

    Global Declare Instance _card_map_inst : _card_map_sig.

    (* Logical function of_seq *)

    Global Declare Instance _of_seq_inst : _of_seq_sig.

    (* Axiom of_seq_multiplicity *)

    Global Declare Instance _of_seq_multiplicity_inst : _of_seq_multiplicity_sig.

  End Bag.

  Module ___Set.

    Import Declarations.___Set.

    (* Logical function __empty *)

    Global Declare Instance ___empty_inst : ___empty_sig.

    (* Logical function __belongs *)

    Global Declare Instance ___belongs_inst : ___belongs_sig.

    (* Logical function __neg_belongs *)

    Global Declare Instance ___neg_belongs_inst : ___neg_belongs_sig.

    (* Logical function mem *)

    Global Declare Instance _mem_inst : _mem_sig.

    (* Axiom mem_fun_def *)

    Global Declare Instance _mem_fun_def_inst : _mem_fun_def_sig.

    (* Axiom nmem_def *)

    Global Declare Instance _nmem_def_inst : _nmem_def_sig.

    (* Axiom empty_mem *)

    Global Declare Instance _empty_mem_inst : _empty_mem_sig.

    (* Logical function add *)

    Global Declare Instance _add_inst : _add_sig.

    (* Axiom add_mem *)

    Global Declare Instance _add_mem_inst : _add_mem_sig.

    (* Axiom add_mem_neq *)

    Global Declare Instance _add_mem_neq_inst : _add_mem_neq_sig.

    (* Logical function __singleton_set *)

    Global Declare Instance ___singleton_set_inst : ___singleton_set_sig.

    (* Logical function singleton *)

    Global Declare Instance _singleton_inst : _singleton_sig.

    (* Axiom singleton_fun_def *)

    Global Declare Instance _singleton_fun_def_inst : _singleton_fun_def_sig.

    (* Axiom singleton_def *)

    Global Declare Instance _singleton_def_inst : _singleton_def_sig.

    (* Logical function remove *)

    Global Declare Instance _remove_inst : _remove_sig.

    (* Axiom remove_mem *)

    Global Declare Instance _remove_mem_inst : _remove_mem_sig.

    (* Axiom remove_mem_neq *)

    Global Declare Instance _remove_mem_neq_inst : _remove_mem_neq_sig.

    (* Logical function __union *)

    Global Declare Instance ___union_inst : ___union_sig.

    (* Axiom union_mem *)

    Global Declare Instance _union_mem_inst : _union_mem_sig.

    (* Axiom union_mem_neg *)

    Global Declare Instance _union_mem_neg_inst : _union_mem_neg_sig.

    (* Logical function __inter *)

    Global Declare Instance ___inter_inst : ___inter_sig.

    (* Axiom inter_mem *)

    Global Declare Instance _inter_mem_inst : _inter_mem_sig.

    (* Axiom inter_mem_neq *)

    Global Declare Instance _inter_mem_neq_inst : _inter_mem_neq_sig.

    (* Logical function disjoint *)

    Global Declare Instance _disjoint_inst : _disjoint_sig.

    (* Axiom disjoint_def *)

    Global Declare Instance _disjoint_def_inst : _disjoint_def_sig.

    (* Logical function __diff *)

    Global Declare Instance ___diff_inst : ___diff_sig.

    (* Axiom diff_mem *)

    Global Declare Instance _diff_mem_inst : _diff_mem_sig.

    (* Axiom diff_mem_fst *)

    Global Declare Instance _diff_mem_fst_inst : _diff_mem_fst_sig.

    (* Logical function __subset *)

    Global Declare Instance ___subset_inst : ___subset_sig.

    (* Axiom subset_def *)

    Global Declare Instance _subset_def_inst : _subset_def_sig.

    (* Logical function map *)

    Global Declare Instance _map_inst : _map_sig.

    (* Axiom set_map *)

    Global Declare Instance _set_map_inst : _set_map_sig.

    (* Logical function partition *)

    Global Declare Instance _partition_inst : _partition_sig.

    (* Axiom partition_l_mem *)

    Global Declare Instance _partition_l_mem_inst : _partition_l_mem_sig.

    (* Axiom partition_r_mem *)

    Global Declare Instance _partition_r_mem_inst : _partition_r_mem_sig.

    (* Logical function cardinal *)

    Global Declare Instance _cardinal_inst : _cardinal_sig.

    (* Logical function finite *)

    Global Declare Instance _finite_inst : _finite_sig.

    (* Axiom finite_def *)

    Global Declare Instance _finite_def_inst : _finite_def_sig.

    (* Axiom cardinal_nonneg *)

    Global Declare Instance _cardinal_nonneg_inst : _cardinal_nonneg_sig.

    (* Axiom cardinal_empty *)

    Global Declare Instance _cardinal_empty_inst : _cardinal_empty_sig.

    (* Axiom cardinal_remove_mem *)

    Global Declare Instance _cardinal_remove_mem_inst : _cardinal_remove_mem_sig.

    (* Axiom cardinal_remove_not_mem *)

    Global Declare Instance _cardinal_remove_not_mem_inst : _cardinal_remove_not_mem_sig.

    (* Axiom cardinal_add *)

    Global Declare Instance _cardinal_add_inst : _cardinal_add_sig.

    (* Axiom cardinal_add_mem *)

    Global Declare Instance _cardinal_add_mem_inst : _cardinal_add_mem_sig.

    (* Logical function of_seq *)

    Global Declare Instance _of_seq_inst : _of_seq_sig.

    (* Axiom of_seq_mem *)

    Global Declare Instance _of_seq_mem_inst : _of_seq_mem_sig.

    (* Logical function to_seq *)

    Global Declare Instance _to_seq_inst : _to_seq_sig.

    (* Axiom to_seq_mem *)

    Global Declare Instance _to_seq_mem_inst : _to_seq_mem_sig.

    (* Logical function fold *)

    Global Declare Instance _fold_inst : _fold_sig.

    (* Axiom fold_def *)

    Global Declare Instance _fold_def_inst : _fold_def_sig.

  End ___Set.

  Module Fin_maps.

    Import Declarations.Fin_maps.

    (* Logical function __empty *)

    Global Declare Instance ___empty_inst : ___empty_sig.

    (* Logical function singleton *)

    Global Declare Instance _singleton_inst : _singleton_sig.

    (* Logical function get *)

    Global Declare Instance _get_inst : _get_sig.

    (* Logical function __seq_get *)

    Global Declare Instance ___seq_get_inst : ___seq_get_sig.

    (* Axiom get_fun_def *)

    Global Declare Instance _get_fun_def_inst : _get_fun_def_sig.

    (* Logical function add *)

    Global Declare Instance _add_inst : _add_sig.

    (* Axiom singleton_def *)

    Global Declare Instance _singleton_def_inst : _singleton_def_sig.

    (* Axiom add_get *)

    Global Declare Instance _add_get_inst : _add_get_sig.

    (* Axiom add_get_neq *)

    Global Declare Instance _add_get_neq_inst : _add_get_neq_sig.

    (* Logical function mem *)

    Global Declare Instance _mem_inst : _mem_sig.

    (* Logical function __belongs *)

    Global Declare Instance ___belongs_inst : ___belongs_sig.

    (* Axiom mem_fun_def *)

    Global Declare Instance _mem_fun_def_inst : _mem_fun_def_sig.

    (* Axiom mem_get *)

    Global Declare Instance _mem_get_inst : _mem_get_sig.

    (* Logical function __neg_belongs *)

    Global Declare Instance ___neg_belongs_inst : ___neg_belongs_sig.

    (* Axiom nmem_def *)

    Global Declare Instance _nmem_def_inst : _nmem_def_sig.

    (* Axiom mem_empty *)

    Global Declare Instance _mem_empty_inst : _mem_empty_sig.

    (* Axiom nmem_add *)

    Global Declare Instance _nmem_add_inst : _nmem_add_sig.

    (* Logical function remove *)

    Global Declare Instance _remove_inst : _remove_sig.

    (* Axiom mem_remove *)

    Global Declare Instance _mem_remove_inst : _mem_remove_sig.

    (* Axiom nmem_remove *)

    Global Declare Instance _nmem_remove_inst : _nmem_remove_sig.

    (* Axiom fmap_extensionality *)

    Global Declare Instance _fmap_extensionality_inst : _fmap_extensionality_sig.

    (* Logical function map *)

    Global Declare Instance _map_inst : _map_sig.

    (* Axiom map_mem *)

    Global Declare Instance _map_mem_inst : _map_mem_sig.

    (* Logical function to_seq *)

    Global Declare Instance _to_seq_inst : _to_seq_sig.

    (* Axiom to_seq_mem *)

    Global Declare Instance _to_seq_mem_inst : _to_seq_mem_sig.

    (* Logical function population *)

    Global Declare Instance _population_inst : _population_sig.

    (* Axiom population_to_seq *)

    Global Declare Instance _population_to_seq_inst : _population_to_seq_sig.

  End Fin_maps.

  (* Logical function __map_set *)

  Global Declare Instance ___map_set_inst : ___map_set_sig.

  (* Axiom map_set_def *)

  Global Declare Instance _map_set_def_inst : _map_set_def_sig.

  (* Axiom map_set_def_neq *)

  Global Declare Instance _map_set_def_neq_inst : _map_set_def_neq_sig.

  Module Map.

    Import Declarations.Map.

    (* Logical function domain *)

    Global Declare Instance _domain_inst : _domain_sig.

    (* Axiom domain_mem *)

    Global Declare Instance _domain_mem_inst : _domain_mem_sig.

  End Map.

End Obligations.