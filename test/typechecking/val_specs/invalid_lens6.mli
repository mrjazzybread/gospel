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
(*@ model left : integer
    model right : prop *)

val f : t -> unit
(*@ () = f x
    consumes x @ Left
    produces x @ Right
    ensures x = old x *)

(* {gospel_expected|
[1] File "invalid_lens6.mli", line 19, characters 20-21:
    19 |     ensures x = old x *)
                             ^
    Error: Mismatch between type integer and type prop
    
|gospel_expected} *)
