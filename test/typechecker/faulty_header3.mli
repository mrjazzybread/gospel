(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

val f : x:('a -> 'b -> 'c) -> y:'a -> 'b -> 'c
(*@ let r = f ~x [z:int] ~y z in 
    ensures true *)

(* {gospel_expected|
   [125] File "faulty_header3.mli", line 12, characters 28-29:
         12 | (*@ let r = f ~x [z:int] ~y z in 
                                          ^
         Error: The variable z is duplicated in this pattern.
   |gospel_expected} *)
