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
(*@ consumes x
    consumes x *)

(* {gospel_expected|
[1] File "./duplicate_consumes_top_level1.mli", line 16, characters 13-14:
    16 |     consumes x *)
                      ^
    Error: The variable x is listed as consumes twice
    
|gospel_expected} *)
