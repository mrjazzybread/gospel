Set Warnings "-deprecated".
Set Warnings "-default".
Set Warnings "-syntax".

Require Import gospelstdlib_mli.

Require Import TLC.LibCore TLC.LibSet.
Require Import TLC.LibListZ.

Local Open Scope comp_scope.

Module Proofs : gospelstdlib_mli.Obligations.

  Import Declarations.

  Global Instance _set_inst : _set_sig :=
    { set := LibSet.set }.

  Global Instance _sequence_inst : _sequence_sig :=
    { sequence := list }.

  Global Instance _bag_inst : _bag_sig :=
    { bag := fun A => A -> nat }.

  Global Instance _option_inst : _option_sig :=
    { option := Datatypes.option }.

  Global Instance _Some_inst : _Some_sig :=
    { Some := fun A _ x => Datatypes.Some x }.

  Global Instance _None_inst : _None_sig :=
    { None := fun A _ => Datatypes.None }.

  Global Instance _succ_inst : _succ_sig :=
    { succ := fun x => x + 1 }.

  Global Instance _pred_inst : _pred_sig :=
    { pred := fun x => x - 1 }.

  Global Instance __mod_inst : __mod_sig :=
    { _mod := Z.modulo }.

  Global Instance _pow_inst : _pow_sig :=
    { pow := Z.pow }.

  Global Instance _abs_inst : _abs_sig :=
    { abs := Z.abs }.

  Global Instance _min_inst : _min_sig :=
    { min := Z.min }.

  Global Instance _max_inst : _max_sig :=
    { max := Z.max }.

  Global Instance _app_inst : _app_sig :=
    { app := fun A _ s1 s2 => s1 ++ s2 }.

  Global Instance _seq_get_inst : _seq_get_sig :=
    { seq_get := fun A _ s i => s[i] }.

  Global Instance _seq_sub_inst : _seq_sub_sig :=
    { seq_sub := fun A _ s i1 i2 => take (i2 - i1) (drop i1 s) }.

  Global Instance _seq_sub_l_inst : _seq_sub_l_sig :=
    { seq_sub_l := fun A _ s i => seq_sub s i (length s) }.

  Global Instance _seq_sub_r_inst : _seq_sub_r_sig :=
    { seq_sub_r := fun A _ s i => seq_sub s (0)%Z i }.

  Global Instance _monoid_inst : _monoid_sig :=
    { monoid := fun A _ f n => neutral_l f n /\ neutral_r f n /\ assoc f }.

  #[refine] Global Instance _monoid_def_inst : _monoid_def_sig := { }.
  Proof.
    intros A Ih f neutral.
    split.
    - intros [H1 [H2 H3]]. split; intros x.
      + rewrite H1. rewrite H2. auto.
      + auto.
    - intros (H1 & H2).
      repeat split; intros x; specialize H1 with x as [H3 H4];
        try (rewrite H3); auto.
  Qed.

  Global Instance _comm_monoid_inst : _comm_monoid_sig :=
    { comm_monoid := fun A _ f n => monoid f n /\ comm f}.

  #[refine] Global Instance _comm_monoid_def_inst : _comm_monoid_def_sig := { }.
  Proof.
    simpl. tauto.
  Qed.

  Module Sequence.

    Import Sequence.

    Global Instance _length_inst : _length_sig :=
      { length := fun A _ l => LibListZ.length l }.

    Global Instance _in_range_inst : _in_range_sig :=
      { in_range := fun A _ s i => 0 <= i < length s }.

    #[refine] Global Instance _in_range_def_inst : _in_range_def_sig := { }.
    Proof.
      simpl. tauto.
    Qed.

    #[refine] Global Instance _length_nonneg_inst : _length_nonneg_sig := { }.
    Proof.
      intros. simpl. math.
    Qed.

    #[refine] Global Instance _append_length_inst : _append_length_sig := { }.
    Proof.
      simpl.
      intros.
      rew_list.
      auto.
    Qed.

    #[refine] Global Instance _append_elems_left_inst : _append_elems_left_sig := { }.
    Proof.
      simpl.
      intros.
      rewrite read_app.
      rewrite If_l.
      + auto.
      + math.
    Qed.

    #[refine] Global Instance _append_elems_right_inst : _append_elems_right_sig := { }.
    Proof.
      simpl.
      intros.
      rewrite read_app.
      rewrite If_r.
      + auto.
      + math.
    Qed.

    #[refine] Global Instance _subseq_l_inst : _subseq_l_sig := { }.
    Proof.
      auto.
    Qed.

    #[refine] Global Instance _subseq_r_inst : _subseq_r_sig := { }.
    Proof.
      auto.
    Qed.

    #[refine] Global Instance _subseq_inst : _subseq_sig := { }.
    Proof.
      simpl.
      intros A IhA s i i1 i2 (H1 & H2 & H3 & H4).
      rewrite read_take; try (split; math).
      - rewrite read_drop; try (split; math).
        f_equal.
        math.
      - rewrite LibListZ.length_drop; math.
    Qed.

    #[refine] Global Instance _subseq_len_inst : _subseq_len_sig := { }.
    Proof.
      simpl.
      intros.
      rewrite LibListZ.length_take. 1: math.
      rewrite LibListZ.length_drop; math.
    Qed.

    Fixpoint init_aux {A} (n : nat) (f : Z -> A) : list A :=
      match n with
      |O => nil
      |S n' => LibList.app (init_aux n' f) (((f n') :: nil)) end.

    Global Instance _init_inst : _init_sig :=
      { init := fun A _ n f =>
          If n < 0 then arbitrary else
          init_aux (Z.to_nat n) f }.

    Lemma init_pos :
      forall A {Ih : Inhab A} n (f : Z -> A),
        n >= 0 -> init n f = init_aux (Z.to_nat n) f.
    Proof.
      simpl.
      intros A Ih n f H.
      unfold init. rewrite If_r. auto. math.
    Qed.


    Lemma init_aux_length :
      forall A n (f : Z -> A),
        LibListZ.length (init_aux n f) = n.
    Proof.
      intros A n f.
      induction n as [|n' Ih].
      all: simpl; rew_list. math.
      rewrite Ih. math.
    Qed.

    #[refine] Global Instance _init_length_inst : _init_length_sig := { }.
    Proof.
      simpl.
      intros A Ih n f H.
      unfold init.
      rewrite If_r.
      - unfold length. rewrite init_aux_length. math.
      - math.
    Qed.

    Lemma init_i :
      forall A (n : nat) (i : Z) (f : Z -> A),
      forall {Ih : Inhab A},
        0 <= i ->
        i < n ->
        (init_aux n f)[i] = f i.
    Proof.
      intros A n i f Inh H1 H2.
      induction n as [|n' Ih].
      - math.
      - simpl. rewrite read_app.
        rewrite init_aux_length.
        assert (P : i < n' \/ ~ i < n').
        apply classic.
        destruct P as [P|P].
        + rewrite If_l. auto. math.
        +  assert (Q : i = n'). { math. } rewrite If_r.
           * rewrite Q. rewrite Z.sub_diag. apply read_zero.
           * math.
    Qed.

    #[refine] Global Instance _init_elems_inst : _init_elems_sig := { }.
    Proof.
      simpl.
      intros A Inh n f i [H1 H2].
      rewrite If_r. 2: try math.
      apply init_i; math.
    Qed.

    Global Instance _empty_inst : _empty_sig :=
      { empty := fun A _ => nil }.

    #[refine] Global Instance _empty_length_inst : _empty_length_sig := { }.
    Proof.
      auto.
    Qed.

    Global Instance _singleton_inst : _singleton_sig :=
      { singleton := fun A _ x => Datatypes.cons x nil }.

    #[refine] Global Instance _singleton_def_inst : _singleton_def_sig := { }.
    Proof.
      simpl.
      intros A IhA x f H1.
      repeat (rewrite If_r; try math;
              simpl; rew_list).
      f_equal.
      rewrite <- H1.
      auto.
    Qed.


    Global Instance _cons_inst : _cons_sig :=
      { cons := fun A _ => List.cons }.

    #[refine] Global Instance _cons_def_inst : _cons_def_sig := { }.
    Proof.
      simpl.
      intros.
      rew_list. auto.
    Qed.

    Global Instance _snoc_inst : _snoc_sig :=
      { snoc := fun A _ s x => s ++ singleton x }.

    #[refine] Global Instance _snoc_def_inst : _snoc_def_sig := { }.
    Proof.
      auto.
    Qed.

    Global Instance _hd_inst : _hd_sig :=
      { hd := fun A _ s => seq_get s 0 }.

    #[refine] Global Instance _hd_def_inst : _hd_def_sig := { }.
    Proof.
      auto.
    Qed.

    Global Instance _tl_inst : _tl_sig :=
      { tl := fun A _ s => List.tl s }.

    #[refine] Global Instance _tl_def_inst : _tl_def_sig := { }.
    Proof.
      simpl.
      intros.
      subst. auto.
    Qed.

    Global Instance _append_inst : _append_sig :=
      { append := fun A _ => app }.

    #[refine] Global Instance _append_def_inst : _append_def_sig := { }.
    Proof.
      auto.
    Qed.

    Global Instance _multiplicity_inst : _multiplicity_sig :=
      { multiplicity := fun A _ e s =>
          LibListZ.count (fun x => x = e) s }.

    #[refine] Global Instance _mult_empty_inst : _mult_empty_sig := { }.
    Proof.
      auto.
    Qed.

    #[refine] Global Instance _mult_cons_inst : _mult_cons_sig := { }.
    Proof.
      simpl.
      intros.
      rew_listx.
      rewrite If_l. 2: auto.
      rewrite Z.add_comm. auto.
    Qed.

    #[refine] Global Instance _mult_cons_neutral_inst : _mult_cons_neutral_sig := { }.
    Proof.
      simpl.
      intros A Ih s x1 x2 H1.
      rew_listx.
      rewrite If_r.
      + math.
      + auto.
    Qed.

    #[refine] Global Instance _mult_length_inst : _mult_length_sig := { }.
    Proof.
      simpl.
      split.
      - apply count_nonneg.
      - induction s as [|e s' Ih].
        + rew_listx. math.
        + rew_listx.
          assert (E : e = x \/ e <> x). apply classic.
          destruct E;
            [rewrite If_l | rewrite If_r]; auto; math.
    Qed.

    Global Instance _mem_inst : _mem_sig :=
      { mem := fun A _ x s => LibList.mem x s }.

    Global Instance _belongs_inst : _belongs_sig :=
      { belongs := fun A _ => mem }.

    #[refine] Global Instance _mem_fun_def_inst : _mem_fun_def_sig := { }.
    Proof.
      tauto.
    Qed.

    #[refine] Global Instance _mem_def_inst : _mem_def_sig := { }.
    Proof.
      simpl.
      intros A IhA s x.
      split; intros H1.
      + change (LibListZ.count (= x) s > 0%nat).
        rewrite <- LibListZ.Exists_eq_count_pos.
        rewrite Exists_eq_exists_mem.
        exists x; auto.
      + change (LibListZ.count (= x) s > 0%nat) in H1.
        rewrite <- LibListZ.Exists_eq_count_pos in H1.
        rewrite Exists_eq_exists_mem in H1.
        destruct H1 as [y [H1 H2]].
        subst. auto.
    Qed.

    Global Instance _neg_belongs_inst : _neg_belongs_sig :=
      { neg_belongs := fun A _ x s => ~ (belongs x s) }.

    #[refine] Global Instance _nmem_def_inst : _nmem_def_sig := { }.
    Proof.
      simpl. tauto.
    Qed.

    Global Instance _Forall_inst : _Forall_sig :=
      { Forall := fun A _ P s => LibList.Forall P s }.

    #[refine] Global Instance _forall_def_inst : _forall_def_sig := { }.
    Proof.
      simpl.
      intros.
      rewrite Forall_eq_forall_mem. tauto.
    Qed.

    Global Instance _Exists_inst : _Exists_sig :=
      { Exists := fun A _ P s => LibList.Exists P s }.

    #[refine] Global Instance _exists_def_inst : _exists_def_sig := { }.
    Proof.
      simpl.
      intros A Ih p s.
      rewrite Exists_eq_exists_mem.
      tauto.
    Qed.

    Global Instance _map_inst : _map_sig :=
      { map := fun A B _ _ => @LibList.map A B }.

    #[refine] Global Instance _map_elems_inst : _map_elems_sig := { }.
    Proof.
      simpl.
      intros A B IhA IhB i f s [H1 H2].
      repeat (rewrite If_r; try math).
      unfold map.
      apply LibListZ.read_map. unfold length in *.
      rew_index. split; auto.
    Qed.

    #[refine] Global Instance _map_length_inst : _map_length_sig := { }.
    Proof.
      simpl.
      intros.
      rew_listx.
      auto.
    Qed.

    Global Instance _filter_inst : _filter_sig :=
      { filter := fun A _ => @LibList.filter A }.

    #[refine] Global Instance _filter_elems_inst : _filter_elems_sig := { }.
    Proof.
      simpl.
      intros A IhA f s.
      unfold mem in *. unfold multiplicity in *.
      induction s as [|e t IHt]; intros x H1 H2.
      - inversion H1.
      - rew_listx.
        inversion H1; subst; auto.
    Qed.

    Global Instance _filter_map_inst : _filter_map_sig :=
      { filter_map :=
          fun A B _ _ f s =>
            let g :=
              fun x =>
                match f x with
                |Datatypes.Some x => x |Datatypes.None => arbitrary end in
            LibList.map g (LibList.filter (fun x => f x <> None) s)
      }.

    #[refine] Global Instance _filter_map_elems_inst : _filter_map_elems_sig := { }.
    Proof.
      simpl.
      intros B A IhB IhA f s y.
      unfold filter_map.
      split; intros H1.
      - destruct H1 as [x [H1 H2]].
        apply mem_map' with x.
        + apply mem_filter; auto.
          rewrite H1. discriminate.
        + rewrite H1. auto.
      - apply LibList.mem_Nth
          with A (LibList.map (*very awkward :|*)
                    (fun x : B =>
                       match f x with
                       | Datatypes.Some x0 => x0
                       | Datatypes.None => arbitrary
                       end)
                    (LibList.filter
                       (fun x : B => f x <> Datatypes.None)
                       s)) y in H1.
        destruct H1 as [n H1].
        apply Nth_map_inv in H1.
        destruct H1 as [x [H1 H2]].
        exists x. apply Nth_mem in H2.
        rewrite mem_filter_eq in H2.
        destruct H2 as [H2 H3].
        split; auto.
        destruct (f x).
        + rewrite H1. auto.
        + contradiction.
    Qed.

    Global Instance _get_inst : _get_sig :=
      { get := fun A _ => seq_get }.

    #[refine] Global Instance _get_def_inst : _get_def_sig := { }.
    Proof.
      auto.
    Qed.

    Fixpoint set_aux {A} (s : sequence A) (n : nat) (x : A) : sequence A :=
      match s, n with
      |nil, _ => arbitrary
      |_ :: t, O => x :: t
      |e :: t, S n' => e :: set_aux t n' x
      end.

    Global Instance _set_inst : _set_sig :=
      { set :=
          fun A _ s i x =>
            If i < 0 then arbitrary else set_aux s (Z.to_nat i) x
      }.

    Lemma set_aux_len :
        forall A (s : sequence A) (i : nat) x,
          0 <= i < Z.to_nat (LibListZ.length s) ->
          LibListZ.length (set_aux s i x) =
            LibListZ.length s.
      Proof.
        intros A s.
        induction s as [|h s Ih];
          intros i x H1;
          rew_listx in H1.
        + math.
        + simpl. rew_listx.
          destruct i; rew_listx; auto.
          rewrite Ih; math.
      Qed.

      Lemma set_aux_elem :
        forall A {IhA : Inhab A} s (i : Z) (x : A),
          0 <= i < Z.to_nat (LibListZ.length s) ->
          (set_aux s (to_nat i) x)[i] = x.
      Proof.
        induction s as [|e t Ih]; intros i x [H1 H2].
        - rew_list in H2. math.
        - simpl. remember (to_nat i) as n eqn:E.
          destruct n as [|n'].
          + assert (A1 : i = 0). { math. }
            rewrite A1. rew_listx. auto.
          + assert (A1 : n' = to_nat (i - 1)).
            { math. }
            rewrite read_cons_pos; try math.
            rewrite A1.
            rew_list in H2.
            rewrite Ih.
            * auto.
            * math.
      Qed.

      #[refine] Global Instance _set_elem_inst : _set_elem_sig := { }.
      Proof.
        simpl.
        intros A IHA s i x [H1 H2].
        unfold set.
        rewrite If_r; try math.
        apply set_aux_elem.
        split; math.
      Qed.

      Lemma set_aux_elem_other :
        forall A {Ih :Inhab A} s i1 i2 (x : A),
          i1 <> i2 ->
          0 <= i1 < LibListZ.length s ->
          0 <= i2 < LibListZ.length s ->
          (set_aux s (Z.to_nat i1) x)[i2] = s[i2].
      Proof.
        intros A IhA s.
        induction s as [|e s Ih]; intros i1 i2 x H1 H2 H3;
          rew_list in *.
        - math.
        - simpl.
          remember (to_nat i1) as n1 eqn:E1.
          remember (to_nat i2) as n2 eqn:E2.
          destruct n1 as [|n1'].
          + destruct n2 as [|n2'].
            * math.
            * rewrite read_cons_pos. 2: try math.
              rewrite read_cons_pos. 2: try math.
              auto.
          + destruct n2 as [|n2'].
            * assert (A1 : i2 = 0). 1: math.
              rewrite A1. rew_list.
              auto.
            * rewrite read_cons_pos. 2: math.
              rewrite read_cons_pos. 2: math.
              assert (A1 : n1' = to_nat (i1 - 1)). 1: math.
              rewrite A1.
              rewrite Ih; auto; math.
      Qed.

      #[refine] Global Instance _set_elem_other_inst : _set_elem_other_sig := { }.
      Proof.
        simpl.
        intros A IhA s i1 i2 x H1 H2 H3.
        repeat (rewrite If_r; try math).
        apply set_aux_elem_other; math.
      Qed.

      Global Instance _rev_inst : _rev_sig :=
        { rev := fun A _ => @LibList.rev A }.

      #[refine] Global Instance _rev_length_inst : _rev_length_sig := { }.
      Proof.
        simpl.
        intros.
        rew_list.
        auto.
      Qed.

            Lemma bah :
        forall A `{Inhab A} (s : sequence A) i,
          0 <= i < LibListZ.length s ->
          s[i] = LibList.nth (abs i) s.
      Proof.
        intros A IhA s i H1.
        remember (abs i) as n eqn:E.
        generalize dependent i.
        generalize dependent s.
        induction n as [|n' Ih]; intros s i H1 H2.
        + assert (A1 : i = 0). { math. } rewrite A1.
          destruct s.
          * rew_list in H1. math.
          * rew_list. rew_listx. auto.
        + destruct s; rew_list in H1.
          *  math.
          * rew_listx.
            rewrite read_cons_pos; try math.
            rewrite Ih; auto; math.
      Qed.

      #[refine] Global Instance _rev_elems_inst : _rev_elems_sig := {}.
      Proof.
        simpl.
        intros A IhA i s [H1 H2].
        unfold seq_get.
        unfold length in *.
        unfold rev.
        rewrite bah.
        rewrite bah.
        + remember (abs i) as n eqn:E1.
          assert
            (A1 : abs (LibListZ.length s - 1 - i) = (LibList.length s - 1 - n)%nat).
          { math. }
          rewrite A1.
          apply nth_rev. math.
        + math.
        + rew_list. math.
      Qed.

      Global Instance _fold_left_inst : _fold_left_sig :=
        { fold_left := fun A B _ _ f x s => LibList.fold_left (fun x y => f y x) x s }.

      #[refine] Global Instance _fold_left_empty_inst : _fold_left_empty_sig := { }.
      Proof.
        simpl.
        intros.
        rew_listx.
        auto.
      Qed.

      #[refine] Global Instance _fold_left_cons_inst : _fold_left_cons_sig := { }.
      Proof.
        simpl.
        intros. rew_listx. auto.
      Qed.

      Global Instance _fold_right_inst : _fold_right_sig :=
        { fold_right := fun A B _ _ f s acc => LibList.fold_right f acc s }.

      #[refine] Global Instance _fold_right_empty_inst : _fold_right_empty_sig := { }.
      Proof.
        simpl.
        intros.
        rew_listx.
        auto.
      Qed.

      #[refine] Global Instance _fold_right_cons_inst : _fold_right_cons_sig := { }.
      Proof.
        simpl.
        intros.
        rew_listx.
        auto.
      Qed.

      #[refine] Global Instance _extensionality_inst : _extensionality_sig := { }.
      Proof.
        simpl.
        intros a IhA s1 s2 H1 H2.
        apply eq_of_extens_range with IhA.
        auto.
        intros n. specialize H2 with n.
        intros H3.
        apply H2. math.
      Qed.

      Global Instance _permut_inst : _permut_sig :=
        { permut :=
            fun A _ s1 s2 =>
              forall x, mem x s1 <-> mem x s2 }.

      #[refine] Global Instance _permut_mem_inst : _permut_mem_sig := { }.
      Proof.
        tauto.
      Qed.

      Global Instance _permut_sub_inst : _permut_sub_sig :=
        { permut_sub :=
            fun A _ s1 s2 i j =>
                permut (seq_sub s1 i j) (seq_sub s2 i j) /\
                  (seq_sub_r s1 i) = (seq_sub_r s2 i) /\
                  (seq_sub_l s1 j) = (seq_sub_l s2 j)
        }.

      #[refine] Global Instance _permut_sub_def_inst : _permut_sub_def_sig := { }.
      Proof.
        simpl.
        tauto.
      Qed.

  End Sequence.

  Module Bag.

    Import Bag.

    Global Instance _multiplicity_inst : _multiplicity_sig :=
      { multiplicity := fun A _ x b => b x }.

    #[refine] Global Instance _well_formed_inst : _well_formed_sig := { }.
    Proof.
      simpl.
      math.
    Qed.

    Global Instance _empty_inst : _empty_sig :=
      { empty := fun A _ _ => 0%nat }.

    #[refine] Global Instance _empty_mult_inst : _empty_mult_sig := { }.
    Proof.
      auto.
    Qed.

    Global Instance _init_inst : _init_sig :=
      { init := fun A _ f => fun x => Z.to_nat (f x) }.

    #[refine] Global Instance _init_axiom_inst : _init_axiom_sig := { }.
    Proof.
      simpl. math.
    Qed.

    Global Instance _add_inst : _add_sig :=
      { add := fun A _ x b => fun y => If y = x then (1 + b y)%nat else b y }.

    #[refine] Global Instance _add_mult_x_inst : _add_mult_x_sig := { }.
    Proof.
      Opaque Z.add.
      simpl.
      intros A IhA b x.
      unfold multiplicity, add.
      rewrite If_l; auto.
      math.
    Qed.

    #[refine] Global Instance _add_mult_neg_x_inst : _add_mult_neg_x_sig := { }.
    Proof.
      simpl.
      intros.
      rewrite If_r; auto.
    Qed.

    Global Instance _singleton_inst : _singleton_sig :=
      { singleton := fun A _ x => fun y => If y = x then 1%nat else 0%nat }.

    Global Instance _singleton_set_inst : _singleton_set_sig :=
      { singleton_set := fun A _ => singleton }.

    #[refine] Global Instance _singleton_fun_def_inst : _singleton_fun_def_sig := { }.
    Proof.
      auto.
    Qed.

    #[refine] Global Instance _singleton_def_inst : _singleton_def_sig := { }.
    Proof.
      auto.
    Qed.

    Global Instance _mem_inst : _mem_sig :=
      { mem := fun A _ x b => multiplicity x b > 0 }.

    Global Instance _belongs_inst : _belongs_sig :=
      { belongs := fun A _ => mem }.

    #[refine] Global Instance _mem_def_inst : _mem_def_sig := { }.
    Proof.
      tauto.
    Qed.

    #[refine] Global Instance _mem_fun_def_inst : _mem_fun_def_sig := { }.
    Proof.
      tauto.
    Qed.

    Global Instance _neg_belongs_inst : _neg_belongs_sig :=
      { neg_belongs := fun A _ x s => not (mem x s) }.

    #[refine] Global Instance _nmem_def_inst : _nmem_def_sig := { }.
    Proof.
      simpl. tauto.
    Qed.

    Global Instance _remove_inst : _remove_sig :=
      { remove := fun A _ x b y => If y = x then Nat.pred (b y) else b y }.

    #[refine] Global Instance _remove_mult_x_inst : _remove_mult_x_sig := { }.
    Proof.
      simpl.
      intros.
      rewrite If_l; auto.
      math.
    Qed.

    #[refine] Global Instance _remove_mult_neg_x_inst : _remove_mult_neg_x_sig := { }.
    Proof.
      simpl.
      intros.
      rewrite If_r; auto.
    Qed.

    Global Instance _union_inst : _union_sig :=
      { union := fun A _ b1 b2 x => Nat.max (b1 x) (b2 x) }.

    #[refine] Global Instance _union_all_inst : _union_all_sig := { }.
    Proof.
      simpl. math.
    Qed.

    Global Instance _sum_inst : _sum_sig :=
      { sum := fun A _ b1 b2 x => (b1 x + b2 x)%nat }.

    #[refine] Global Instance _sum_all_inst : _sum_all_sig := { }.
    Proof.
      simpl. math.
    Qed.

    Global Instance _inter_inst : _inter_sig :=
      { inter := fun A Ih b1 b2 => fun x => Nat.min (b1 x) (b2 x) }.

    #[refine] Global Instance _inter_all_inst : _inter_all_sig := { }.
    Proof.
      simpl. math.
    Qed.

    Global Instance _diff_inst : _diff_sig :=
      { diff := fun A _ b1 b2 x => (b1 x - b2 x)%nat }.

    #[refine] Global Instance _diff_all_inst : _diff_all_sig := { }.
    Proof.
      simpl. math.
    Qed.

    Global Instance _disjoint_inst : _disjoint_sig :=
      { disjoint := fun A _ b1 b2 => forall x, mem x b1 -> not (mem x b2) }.

    #[refine] Global Instance _disjoint_def_inst : _disjoint_def_sig := { }.
    Proof.
      tauto.
    Qed.

    Global Instance _subset_inst : _subset_sig :=
      { subset := fun A _ b1 b2 => forall x, multiplicity x b1 <= multiplicity x b2 }.

    #[refine] Global Instance _subset_def_inst : _subset_def_sig := { }.
    Proof.
      tauto.
    Qed.

    Global Instance _filter_inst : _filter_sig :=
      { filter := fun A _ P b => fun x => If P x then b x else 0%nat }.

    #[refine] Global Instance _filter_mem_inst : _filter_mem_sig := { }.
    Proof.
      simpl.
      intros.
      rewrite If_l; auto.
    Qed.

    #[refine] Global Instance _filter_mem_neg_inst : _filter_mem_neg_sig := { }.
    Proof.
      simpl.
      intros.
      rewrite If_r; auto.
    Qed.

    Definition cover {A} (b : bag A) l :=
        forall x, b x = LibList.count (fun y => y = x) l.

    Import LibEpsilon.

    Global Instance _cardinal_inst : _cardinal_sig :=
      { cardinal := fun A _ b => epsilon (fun n : nat => exists l, cover b l /\ n = LibList.length l) }.

    Global Instance _finite_inst : _finite_sig :=
      { finite :=
          fun A _ b =>
            exists l, forall x,
              mem x b -> Sequence.mem x l }.

    #[refine] Global Instance _finite_def_inst : _finite_def_sig := { }.
    Proof.
      tauto.
    Qed.

    Global Declare Instance _card_nonneg_inst : _card_nonneg_sig.
    Global Declare Instance _card_empty_inst : _card_empty_sig.
    Global Declare Instance _card_singleton_inst : _card_singleton_sig.
    Global Declare Instance _card_union_inst : _card_union_sig.
    Global Declare Instance _card_add_inst : _card_add_sig.
    Global Declare Instance _card_map_inst : _card_map_sig.

    Global Instance _of_seq_inst : _of_seq_sig :=
      { of_seq := fun A _ s => fun x => LibList.count (= x) s }.

    #[refine] Global Instance _of_seq_multiplicity_inst : _of_seq_multiplicity_sig := { }.
    Proof.
      simpl.
      unfold LibListZ.count.
      math.
    Qed.

  End Bag.

  Module _Set.
    Import _Set.
    Import LibSet.

    Global Instance _mem_inst : _mem_sig :=
      { mem := fun A _ x (s : set A) => x \in s }.

    Global Instance _belongs_inst : _belongs_sig :=
      { belongs := fun A _ => mem }.

    Global Instance _neg_belongs_inst : _neg_belongs_sig :=
      { neg_belongs := fun A _ x s => ~ belongs x s }.

    #[refine] Global Instance _mem_fun_def_inst : _mem_fun_def_sig := { }.
    Proof.
      tauto.
    Qed.

    #[refine] Global Instance _nmem_def_inst : _nmem_def_sig := { }.
    Proof.
      simpl. tauto.
    Qed.

    Global Instance _empty_inst : _empty_sig :=
      { empty := fun A _ => LibContainer.empty }.

    #[refine] Global Instance _empty_mem_inst : _empty_mem_sig := { }.
    Proof.
      simpl.
      auto.
    Qed.

    Global Instance _add_inst : _add_sig :=
      { add := fun A _ x s => s \u (single x) }.

    #[refine] Global Instance _add_mem_inst : _add_mem_sig := { }.
    Proof.
      simpl.
      intros.
      rew_set.
      auto.
    Qed.

    #[refine] Global Instance _add_mem_neq_inst : _add_mem_neq_sig := { }.
    Proof.
      simpl.
      intros.
      rew_set.
      tauto.
    Qed.

    Global Instance _singleton_inst : _singleton_sig :=
      { singleton := fun A _ x => single x }.

    Global Instance _singleton_set_inst : _singleton_set_sig :=
      { singleton_set := fun A _ => singleton }.

    #[refine] Global Instance _singleton_def_inst : _singleton_def_sig := { }.
    Proof.
      simpl. intros.
      rewrite union_empty_l. auto.
    Qed.

    #[refine] Global Instance _singleton_fun_def_inst : _singleton_fun_def_sig := { }.
    Proof.
      tauto.
    Qed.

    Global Instance _remove_inst : _remove_sig :=
      { remove := fun A _ x s => s \-- x }.

    #[refine] Global Instance _remove_mem_inst : _remove_mem_sig := { }.
    Proof.
      simpl.
      intros A Ih s x.
      rewrite set_in_remove_eq.
      rew_set. tauto.
    Qed.

    #[refine] Global Instance _remove_mem_neq_inst : _remove_mem_neq_sig := { }.
    Proof.
      simpl.
      intros.
      rew_set. tauto.
    Qed.

    Global Instance _union_inst : _union_sig :=
      { union := fun A _ s1 s2 => s1 \u s2 }.

    #[refine] Global Instance _union_mem_inst : _union_mem_sig := { }.
    Proof.
      auto.
    Qed.

    #[refine] Global Instance _union_mem_neg_inst : _union_mem_neg_sig := { }.
    Proof.
      simpl.
      intros.
      rew_set. tauto.
    Qed.

    Global Instance _inter_inst : _inter_sig :=
      { inter := fun A _ s1 s2 => s1 \n s2 }.

    #[refine] Global Instance _inter_mem_inst : _inter_mem_sig := { }.
    Proof.
      simpl. intros.
      rew_set. auto.
    Qed.

    #[refine] Global Instance _inter_mem_neq_inst : _inter_mem_neq_sig := { }.
    Proof.
      simpl.
      intros.
      rew_set. tauto.
    Qed.

    Global Instance _disjoint_inst : _disjoint_sig :=
      { disjoint := fun A _ s1 s2 => inter s1 s2 = empty }.

    #[refine] Global Instance _disjoint_def_inst : _disjoint_def_sig := { }.
    Proof.
      tauto.
    Qed.

    Global Instance _diff_inst : _diff_sig :=
      { diff := fun A _ s1 s2 => LibContainer.remove s1 s2 }.

    #[refine] Global Instance _diff_mem_inst : _diff_mem_sig := { }.
    Proof.
      simpl.
      intros A Ih s1 s2 x H1.
      rew_set.
      intros [H2 H3].
      contradiction.
    Qed.

    #[refine] Global Instance _diff_mem_fst_inst : _diff_mem_fst_sig := { }.
    Proof.
      simpl.
      intros.
      rew_set. tauto.
    Qed.

    Global Instance _subset_inst : _subset_sig :=
      { subset := fun A _ s1 s2 => forall x, mem x s1 -> mem x s2 }.

    #[refine] Global Instance _subset_def_inst : _subset_def_sig := { }.
    Proof.
      tauto.
    Qed.

    Global Instance _map_inst : _map_sig :=
      { map := fun A B _ _ f (s : set A) x =>
                 exists (y : A), f y = x /\ (y \in s) }.

    #[refine] Global Instance _set_map_inst : _set_map_sig := { }.
    Proof.
      simpl.
      intros.
      tauto.
    Qed.

    Global Instance _partition_inst : _partition_sig :=
      { partition :=
          fun A _ p (s : set A) =>
            (set_st (fun x => p x /\ x \in s),
              set_st (fun x => ~p x /\ x \in s)) }.

    #[refine] Global Instance _partition_l_mem_inst : _partition_l_mem_sig := { }.
    Proof.
      simpl.
      intros A Ih f s x p1 p2 H1 H2 H3.
      injection H3.
      intros. subst. rew_set. auto.
    Qed.

    #[refine] Global Instance _partition_r_mem_inst : _partition_r_mem_sig := { }.
    Proof.
      simpl.
      intros A Ih f s x p1 p2 H1 H2 H3.
      injection H3.
      intros. subst.
      rew_set. auto.
    Qed.

    Global Instance _cardinal_inst : _cardinal_sig :=
      { cardinal := fun A _ s => Z.of_nat (card s) }.

    Global Instance _finite_inst : _finite_sig :=
      { finite := fun A _ s => LibSet.finite s }.

    #[refine] Global Instance _finite_def_inst : _finite_def_sig := { }.
    Proof.
      tauto.
    Qed.

    #[refine] Global Instance _cardinal_nonneg_inst : _cardinal_nonneg_sig := { }.
    Proof.
      simpl. intros. math.
    Qed.

    #[refine] Global Instance _cardinal_empty_inst : _cardinal_empty_sig := { }.
    Proof.
      simpl.
      intros.
      rewrite card_empty. math.
    Qed.

    #[refine] Global Instance _cardinal_remove_mem_inst : _cardinal_remove_mem_sig := { }.
    Proof.
      simpl.
      intros.
      rewrite card_diff_single. 2, 3: auto.
      assert (Q : 1%nat <= card s).
      { apply card_ge_one with x; auto. }
      math.
    Qed.

    #[refine] Global Instance _cardinal_remove_not_mem_inst : _cardinal_remove_not_mem_sig := { }.
    Proof.
      simpl.
      intros A Ih s x H1 H2.
      repeat f_equal.
      rew_set.
      intro y.
      rew_set.
      split; intros H3.
      + tauto.
      + split. 1: auto.
        intros H4. subst. auto.
    Qed.

    #[refine] Global Instance _cardinal_add_inst : _cardinal_add_sig := { }.
    Proof.
      simpl.
      intros.
      rewrite card_disjoint_union_single. 2, 3: auto. math.
    Qed.

    #[refine] Global Instance _cardinal_add_mem_inst : _cardinal_add_mem_sig := { }.
    Proof.
      simpl.
      intros.
      repeat f_equal.
      rew_set.
      split; intros; rew_set in *. 2: auto.
      destruct H1; subst; auto.
    Qed.

    Global Instance _of_seq_inst : _of_seq_sig :=
      { of_seq := fun A _ s => fun x => LibList.mem x s }.

    #[refine] Global Instance _of_seq_mem_inst : _of_seq_mem_sig := { }.
    Proof.
      tauto.
    Qed.

    Global Instance _to_seq_inst : _to_seq_sig :=
      { to_seq := fun A _ s => LibSet.to_list s }.

          Lemma count_no_dup :
        forall A (x : A) l,
          noduplicates l ->
          count (=x) l = 1 <-> LibList.mem x l.
      Proof.
        intros A x l H1.
        split; intros H2.
        - assert (A1 : count (=x) l > 0). { math. }
          apply exists_mem_of_count_pos in A1.
          destruct A1 as [y [H3 H4]]. subst. assumption.
        - induction H1 as [|h l Ih1 Ih2 Ih3].
          + inversion H2.
          + rew_listx in *.
            destruct H2 as [H2 | H2].
            * symmetry in H2. subst. rewrite If_l; auto.
              assert (A2 : ~ count (=x) l > 0).
              { intros H2. rewrite <- Exists_eq_count_pos in H2.
                rewrite Exists_eq_exists_mem in H2.
                destruct H2 as [y [H2 H3]]. subst. contradiction.
              }
              assert (A3 : ~ count (=x) l < 0).
              { unfold count. math. }
              math.
            * rewrite If_r.
              { rewrite Ih3. lia. assumption. }
              { intros H4. subst. contradiction. }
      Qed.

      #[refine] Global Instance _to_seq_mem_inst : _to_seq_mem_sig := { }.
      Proof.
        simpl.
        intros A Ih s F x.
        remember (to_list s) as l eqn:E.
        apply eq_to_list_inv in E; auto.
        unfold list_repr in E.
        destruct E as [E1 E2].
        symmetry.
        rewrite count_no_dup; auto.
      Qed.

      Import LibMonoid.
      Global Instance _fold_inst : _fold_sig :=
        { fold := fun A B _ _ f m s acc =>
            let monoid :=
              {|
                monoid_oper := m;
                monoid_neutral := acc;
              |} in
            LibContainer.fold monoid f s }.

      #[refine] Global Instance _fold_def_inst : _fold_def_sig := { }.
      Proof.
        simpl.
        intros A B IhA IhB f m s acc F [[H1 [H2 H3]] H4].
        remember ({|
                      monoid_oper := m;
                      monoid_neutral := acc
                    |}) as op eqn:E.
        rewrite fold_eq_fold_list_repr with
          (A:=A) (B:=B) (m:=op) (f:=f) (E:=s) (L:=to_list s).
        - induction (to_list s) as [|h t Ih].
          + rew_listx. subst. auto.
          + rew_listx. rewrite Ih.
            subst. auto.
        - repeat split; rewrite E; auto.
        - unfold to_seq. apply list_repr_to_list_of_finite. auto.
      Qed.

  End _Set.

  Global Instance _map_set_inst : _map_set_sig :=
    { map_set :=
        fun A B _ _ f x y =>
        fun arg : A =>
          if classicT (arg = x) then y else f arg }.

  #[refine] Global Instance _map_set_def_inst : _map_set_def_sig := { }.
  Proof.
    simpl.
    intros.
    rewrite If_l; auto.
  Qed.

  #[refine] Global Instance _map_set_def_neq_inst : _map_set_def_neq_sig := { }.
  Proof.
    simpl.
    intros. rewrite If_r; auto.
  Qed.

  Module Map.

    Import Map.

    Global Instance _domain_inst : _domain_sig :=
      { domain := fun A B _ _ d m => set_st (fun x => m x <> d) }.

    #[refine] Global Instance _domain_mem_inst : _domain_mem_sig := { }.
    Proof.
      simpl.
      intros.
      rewrite in_set_st_eq.
      auto.
    Qed.
  End Map.
End Proofs.
