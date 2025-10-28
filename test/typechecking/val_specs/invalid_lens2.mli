(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

type t1
(*@ model : integer *)

type t2
(*@ model : integer *)

val fail : t1 -> t2
(*@ x = fail y
    modifies y @ T2 *)
(* {gospel_expected|
[1] File "invalid_lens2.mli", line 19, characters 15-19:
    19 |     modifies y @ T2 *)
                        ^^^^
    Error: Mismatch between type t2 and type t1
    
|gospel_expected} *)
