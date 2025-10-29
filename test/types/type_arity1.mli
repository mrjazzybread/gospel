(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(*@ type t1 *)

(*@ predicate test (x : 'a t1) *)

(* {gospel_expected|
[1] File "type_arity1.mli", line 11, characters 9-11:
    11 | (*@ type t1 *)
                  ^^
    Error: The type constructor t1 expected 0 argument(s)
           but is applied to 1 argument(s) here
    
|gospel_expected} *)
