Require gospelstdlib_mli gospelstdlib_proof Gospel.primitives ocamlprimitives_mli ocamlprimitives_proof.
Include gospelstdlib_mli.Declarations.

Module Stdlib.
  Include gospelstdlib_mli.Declarations.
End Stdlib.

Include primitives.
Include heap.

Module Primitives (Heap : H).
  Module M := ocamlprimitives_proof.Proofs Heap.
  Include M.Declarations.
  Include M.
End Primitives.

Require Import iris.proofmode.proofmode iris.heap_lang.proofmode iris.heap_lang.notation iris.prelude.options.
