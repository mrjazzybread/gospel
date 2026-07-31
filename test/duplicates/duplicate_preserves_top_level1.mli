(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

val x : int ref
(*@ ensures True *)

val f : unit -> unit
(*@ preserves x
    preserves x *)

(* {gospel_expected|
[1] File "./duplicate_preserves_top_level1.mli", line 16, characters 14-15:
    16 |     preserves x *)
                       ^
    Error: The variable x is listed as preserved twice
    
|gospel_expected} *)
