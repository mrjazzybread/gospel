(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

type t
(*@ model : integer *)

val fail : t -> t
(*@ x = fail y
    modifies y @ t *)

(* {gospel_expected|
[1] File "invalid_lens1.mli", line 16, characters 17-18:
    16 |     modifies y @ t *)
                          ^
    Error: Unbound lens t
    
|gospel_expected} *)
