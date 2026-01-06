Require Import gospelstdlib_mli Gospel.primitives heap.

Require gospelstdlib_mli gospelstdlib_proof.

Import gospelstdlib_mli.Declarations.

Local Open Scope Z_scope.

Require Import
  Stdlib.Floats.Floats
  Stdlib.ZArith.BinIntDef
  Stdlib.Strings.Ascii.

Require Import
  iris.proofmode.proofmode
  iris.heap_lang.proofmode
  iris.heap_lang.notation
  iris.prelude.options.

Module Declarations (Heap : H) .

  Import Heap.

  Class _Unit_sig  := { Unit : val -> unit -> iProp }.

  Class _Int_sig  := { Int : val -> integer -> iProp }.

  Class _Bool_sig  := { Bool : val -> prop -> iProp }.

  Class _Char_sig  := { Char : val -> char -> iProp }.

  Class _Option_sig  := { Option : val -> (option val) -> iProp }.

  Class _List_sig  := { List : val -> (sequence val) -> iProp }.

  Record array := {
    elems : sequence val; length : integer
  }.

  Class _Array_sig  := { Array : val -> array -> iProp }.

  Class _Length_sig  := { Length : val -> integer -> iProp }.

  Class _String_sig  := { String : val -> (sequence char) -> iProp }.

  Class _Bytes_sig  := { Bytes : val -> (sequence char) -> iProp }.

  Class _Ref_sig  := { Ref : val -> val -> iProp }.

End Declarations.

Module Type Obligations (Heap : H) .

  Module Declarations := Declarations
Heap.

  Import Declarations.

  Global Declare Instance _Unit_inst : _Unit_sig.

  Global Declare Instance _Int_inst : _Int_sig.

  Global Declare Instance _Bool_inst : _Bool_sig.

  Global Declare Instance _Char_inst : _Char_sig.

  Global Declare Instance _Option_inst : _Option_sig.

  Global Declare Instance _List_inst : _List_sig.

  Global Declare Instance _Array_inst : _Array_sig.

  Global Declare Instance _Length_inst : _Length_sig.

  Global Declare Instance _String_inst : _String_sig.

  Global Declare Instance _Bytes_inst : _Bytes_sig.

  Global Declare Instance _Ref_inst : _Ref_sig.

End Obligations.