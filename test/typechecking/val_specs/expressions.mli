(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

val f : int -> int -> int
(*@ r = f x y
    requires x > 0
    requires y + 2 < 0
    requires x + 1 < 0
    ensures r = x + y
    ensures r > 2
    ensures r = 3 *)

exception X
exception Y of int

val f : int -> int -> int
(*@ r = f x y
    requires x > 0
    requires y + 2 < 0
    requires x + 1 < 0
    ensures r = x + y
    ensures r > 2
    ensures r = 3
    raises X
      ensures x = 2
    raises Y i
      ensures i = y + 2 -3 / x *)
