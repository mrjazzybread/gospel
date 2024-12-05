(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

exception E of int * int

(*@ function integer_of_int (x:int): integer *)
(*@ function fst (x: 'a * 'a): 'a *)

val f : 'a -> 'a
(*@ match f y with
    | exception E x -> 
       ensures integer_of_int (fst x) = 1 
*)

(* ERROR:
   exception E has two arguments
   TODO reuse constructor's error message *)

(* {gospel_expected|
   [125] Error: Type checking error: Exception pattern has 1 arguments but expected 2.
   |gospel_expected} *)
