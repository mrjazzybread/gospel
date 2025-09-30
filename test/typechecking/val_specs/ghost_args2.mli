(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(*@ function fibonacci (n: integer) : integer *)

val fib : int -> int -> int -> int
(*@ r = fib [i: integer] n a b
    requires i >= 0
    checks n >= 0
    requires a = fibonacci i
    requires b = fibonacci (i+1)
    ensures r = fibonacci (i+n) *)
