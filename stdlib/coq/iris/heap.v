Require Import iris.proofmode.proofmode iris.heap_lang.proofmode iris.heap_lang.notation iris.prelude.options.

Module Type H.
  Parameter Σ : gFunctors.
  Declare Instance h : heapGS Σ.
  Notation iProp := (iProp Σ).
End H.
