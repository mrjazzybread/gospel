(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(*@ type 'a t1 *)

(*@ predicate test (x : t1) *)

(* {gospel_expected|
[1] File "type_arity4.mli", line 13, characters 24-26:
    13 | (*@ predicate test (x : t1) *)
                                 ^^
    Error: The type constructor t1 expected 1 argument(s)
           but is applied to 0 argument(s) here
    
|gospel_expected} *)
