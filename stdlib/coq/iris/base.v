Require gospelstdlib_mli_stdpp gospelstdlib_verified_stdpp Gospel.primitives.
Include gospelstdlib_mli_stdpp.
Include gospelstdlib_verified_stdpp.
Include primitives.
Require Import iris.proofmode.proofmode iris.heap_lang.proofmode iris.heap_lang.notation iris.prelude.options.

Module Type H.
  Parameter Σ : gFunctors.
  Declare Instance h : heapGS Σ.
  Notation iProp := (iProp Σ).
End H.
