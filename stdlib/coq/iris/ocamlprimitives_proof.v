Require Import gospelstdlib_mli.
Require Import gospelstdlib_proof.
Require Import heap.

Require Import iris.proofmode.proofmode iris.heap_lang.proofmode iris.heap_lang.notation iris.prelude.options.

Require Import ocamlprimitives_mli.

Open Scope I.
Open Scope Z_scope.

Module Proofs (Heap : H).

  Import Heap.

  Definition Val (v1 : val) (v2 : val) : iProp := ⌜ v1 = v2 ⌝.
  Module Declarations := Declarations Heap.

  Module Proofs' <: ocamlprimitives_mli.Obligations Heap.

    Module Declarations := Declarations.
    Import Declarations.

    Global Instance _Unit_inst : _Unit_sig :=
      { Unit := fun v m => ⌜ v = #() /\ m = tt ⌝ }.

    Global Instance _Int_inst : _Int_sig :=
      { Int := fun v m => ⌜ v = #m ⌝ }.

    Global Instance _Bool_inst : _Bool_sig :=
      { Bool := fun v m => ⌜ v = #true <-> m ⌝  }.

    Global Declare Instance _Char_inst : _Char_sig.

    Definition Option' v m : iProp :=
      match m with
      | Datatypes.None => ⌜ v = NONEV ⌝
      | Datatypes.Some m' => ⌜ v = SOMEV m' ⌝ end.

    Global Instance _Option_inst : _Option_sig :=
      { Option := Option' }.

    Fixpoint List' (v : val) (m : list val) : iProp :=
      match m with
      |nil => ⌜ v = NONEV ⌝
      |x :: t => ∃ v', ⌜ v = InjRV (x, v') ⌝ ∗ List' v' t end.

    Global Instance _List_inst : _List_sig :=
      { List := List' }.

    Definition Length' (v : val) (size : Z) : Prop :=
      ∃ (l : loc) (n : Z), v = (#l, #n)%V /\ size = n.

    Global Instance _Length_inst : _Length_sig :=
      { Length := fun v s => ⌜ Length' v s ⌝ }.

    Fixpoint heap_array (l : loc) (model : list val) :=
      match model with
      | [] => True
      | h :: t => l ↦ h ∗ heap_array (l +ₗ 1) t
      end.

    Definition Array' (v : val) (arr : array) : iProp :=
      ∃ (l : loc) (n : Z),
        ⌜ v = (#l, #n)%V ⌝ ∗ ⌜ List.length (elems arr) = Z.to_nat (length arr) ⌝ ∗ heap_array l (elems arr) ∗ Length v (length arr).

    Global Instance _Array_inst : _Array_sig :=
      { Array := Array' }.

    Global Declare Instance _String_inst : _String_sig.

    Global Declare Instance _Bytes_inst : _Bytes_sig.

    Global Instance _Ref_inst : _Ref_sig :=
      { Ref := fun r c => ∃ (l : loc), ⌜ r = #l ⌝ ∗ l ↦ c }.

  End Proofs'.

End Proofs.
