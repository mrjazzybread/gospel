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

(*@ predicate test (x : ('a, 'b) t1) *)

(* {gospel_expected|
[1] File "type_arity3.mli", line 11, characters 12-14:
    11 | (*@ type 'a t1 *)
                     ^^
    Error: The type constructor t1 expected 1 argument(s)
           but is applied to 2 argument(s) here
    
|gospel_expected} *)
