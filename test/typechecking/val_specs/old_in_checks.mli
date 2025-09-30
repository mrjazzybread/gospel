(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

val first : 'a array -> 'a
(*@ x = first a
    checks Sequence.length (old a) >= 1 *)

(* {gospel_expected|
[1] File "old_in_checks.mli", line 13, characters 32-33:
    13 |     checks Sequence.length (old a) >= 1 *)
                                         ^
    Error: The old tag cannot be used in this term
    
|gospel_expected} *)
