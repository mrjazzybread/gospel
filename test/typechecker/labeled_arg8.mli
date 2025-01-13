(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

val f : x:('a -> 'b -> 'c) -> 'a -> 'b -> 'c
(*@ let r = f x y z in
    ensures true *)

(* ERROR:
   named parameter not specified in function header *)

(* {gospel_expected|
   [125] File "labeled_arg8.mli", line 12, characters 14-15:
         12 | (*@ let r = f x y z in
                            ^
         Error: Type checking error: parameter does not match with val type.
   |gospel_expected} *)
