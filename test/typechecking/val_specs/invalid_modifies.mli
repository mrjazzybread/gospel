(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

val x : int -> int
val invalid_modifies : unit -> unit
(*@ invalid_modifies ()
    modifies x *)
(* {gospel_expected|
[1] File "invalid_modifies.mli", line 14, characters 13-14:
    14 |     modifies x *)
                      ^
    Error: The variable x cannot appear in an ownership clause
    
|gospel_expected} *)
