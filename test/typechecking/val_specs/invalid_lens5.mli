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
(*@ mutable model left : integer
    model right : integer *)

val f : t -> unit
(*@ () = f x
    modifies x @ Left *)

(* {gospel_expected|
[1] File "invalid_lens5.mli", line 17, characters 17-21:
    17 |     modifies x @ Left *)
                          ^^^^
    Error: Unbound lens Left
    
|gospel_expected} *)
