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
(*@ mutable model view : integer *)

val f : t -> unit
(*@ () = f x
    modifies x @ View *)

(* {gospel_expected|
[1] File "invalid_lens4.mli", line 16, characters 17-21:
    16 |     modifies x @ View *)
                          ^^^^
    Error: Unbound lens View
    
|gospel_expected} *)
