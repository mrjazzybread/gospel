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
(*@requires x > 0
    requires y + 2 < 0
    requires x + 1 < 0
    let r = f x y in
    ensures r = x + y
    ensures r > 2
    ensures r = 3 *)

exception X
exception Y of int

val f : int -> int -> int
(*@ requires x > 0
    requires y + 2 < 0
    requires x + 1 < 0
    match f x y with
    |r -> ensures r = x + y
          ensures r > 2
          ensures r = 3
    |exception X -> ensures x = 2
    |exception Y i -> ensures i = y + 2 -3 / x *)
