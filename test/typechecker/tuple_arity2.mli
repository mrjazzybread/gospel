(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

exception E of (int * int * int)

(*@ function integer_of_int (x:int): integer *)
(*@ function fst (x: 'a * 'a): 'a *)

val f : 'a -> 'a
(*@ x = f y
    raises E (x,y) -> integer_of_int x = 1
*)

(* {gospel_expected|
   [125] File "tuple_arity2.mli", line 18, characters 11-42:
         18 |     raises E (x,y) -> integer_of_int x = 1
                         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
         Error: Type checking error: Exception pattern has 2 arguments but expected 1.
   |gospel_expected} *)
