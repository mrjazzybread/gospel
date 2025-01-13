(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

val f : y:int -> int -> int
(*@ let r = f ?y x in
    ensures true *)

(* ERROR:
   the first parameter is named but is defined as optional in spec header *)

(* {gospel_expected|
   [125] File "labeled_arg6.mli", line 12, characters 15-16:
         12 | (*@ let r = f ?y x in
                             ^
         Error: Type checking error: parameter does not match with val type.
   |gospel_expected} *)
