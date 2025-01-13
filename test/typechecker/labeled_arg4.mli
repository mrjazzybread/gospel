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
(*@ let r = f ~y y in
    ensures true *)

(* ERROR:
   Line 12
   duplicated vars in val header
   remove replace the second y by z in line 12 *)

(* {gospel_expected|
   [125] File "labeled_arg4.mli", line 12, characters 17-18:
         12 | (*@ let r = f ~y y in
                               ^
         Error: The variable y is duplicated in this pattern.
   |gospel_expected} *)
