(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

val fail : int -> int
(*@ y = fail x
    preserves x @ Bool *)

(* {gospel_expected|
[1] File "invalid_lens3.mli", line 13, characters 16-22:
    13 |     preserves x @ Bool *)
                         ^^^^^^
    Error: Mismatch between type bool and type int
    
|gospel_expected} *)
