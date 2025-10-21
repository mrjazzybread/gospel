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
(*@ mutable *)

type t2
(*@ mutable *)

val f : unit -> t1 * t2
(*@ x, y = f ()
    produces x
    produces y
    ensures (x : val) = (y : val) *)
