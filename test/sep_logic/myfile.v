Set Implicit Arguments.

Require Coq.ZArith.BinInt TLC.LibLogic TLC.LibRelation TLC.LibInt TLC.LibListZ.

Require CFML.SepBase CFML.SepLifted CFML.WPLifted CFML.WPRecord CFML.WPArray CFML.WPBuiltin.

Require CFML.Stdlib.Array_ml CFML.Stdlib.List_ml CFML.Stdlib.Sys_ml.

Require Import Coq.ZArith.BinIntDef CFML.Semantics CFML.WPHeader.

Delimit Scope Z_scope with Z.

Existing Instance WPHeader.Enc_any | 99.

Parameter rev_spec :
  forall A : Type,
  forall EA : CFML.SepLifted.Enc A,
  forall l : list A,
  CFML.SepLifted.Triple (
    CFML.SepLifted.Trm_apps rev (
      Coq.Lists.List.cons (@CFML.SepLifted.dyn_make (list A) _ l) Coq.Lists.List.nil
    )
  ) (
    CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hpure (
      Coq.Init.Logic.not (Coq.Init.Logic.eq l (@nil A))
    )
  ) (
    fun l' : list A =>
    CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hpure (
      Coq.Init.Logic.not (Coq.Init.Logic.eq res (@nil A))
    )
  ).

