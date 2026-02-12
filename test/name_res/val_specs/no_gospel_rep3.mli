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

val f : t1 -> t2 -> unit
(*@ f x y
    modifies x
    modifies y
    requires (x : val) = (y : val) *)
