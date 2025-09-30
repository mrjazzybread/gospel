(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(** Tests for constant literals *)

val f : int -> float
(*@ y = f x
    requires x = 0
    ensures y = 0. *)

val g : char -> string
(*@ y = g x
    requires x = 'c'
    ensures y[0] = 'c' *)
