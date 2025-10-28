(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

val f1 : int -> int
(*@ x = f1 y
    preserves y @ Int
    produces x @ Int *)

val f2 : int -> bool
(*@ x = f2 y
    preserves y @ Int
    produces x @ Bool *)

val f3 : bool -> int
(*@ x = f3 y
    preserves y @ Bool
    produces x @ Int *)

type t1
(*@ model : integer *)

val f4 : t1 -> int
(*@ x = f4 y
    preserves y @ T1
    produces x @ Int
    ensures x = y *)

val f5 : t1 -> t1
(*@ x = f5 y
    preserves y @ T1
    produces x @ T1
    ensures x = y *)

type t2
(*@ mutable model : integer *)

val f6 : t2 -> t1
(*@ x = f6 y
    preserves y @ T2
    produces x @ T1 *)

val f7 : t2 -> t1
(*@ x = f7 y
    modifies y @ T2
    produces x @ T1 *)
