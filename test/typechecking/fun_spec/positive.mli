(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(*@ function p (x:integer):integer = x
      requires x = 1
      variant x
      ensures x = 2
      ensures x > 2
      ensures x > 1 *)

(*@ function rec f (x : bool): bool = f x *)

(*@ function rec f (x : bool) (y : integer): prop = f x y *)

(*@ function g (a : integer): integer =
      if (f true a) then 1 else 2 *)

(*@ function int_of_integer (x : integer) : integer *)

(*@ function h (a : integer) (b : bool) (c : 'a) : prop =
      if (a = int_of_integer 2)
      then f b (int_of_integer 3)
      else g (int_of_integer 4) = 5 *)

(*@ function h: bool = [@ athing]true *)

(*@ function to_integer (i : integer) : integer *)

(*@ function i (a : integer) : integer =
      int_of_integer (to_integer a + 1) *)

(*@ function i (a : integer) : integer = int_of_integer (to_integer a + 1)
    requires to_integer a > 0
    ensures
      let a' = to_integer a in
      to_integer a = a' + 1 *)

(*@ function gnr : 'a *)

(*@ function g (x y : 'a) (i : integer): 'a *)

(*@ axiom ax1: forall x y. y = f x *)

val f : int -> int -> int
(*@ r = f x y
    requires x > 0
    requires y + 2 < 0
    ensures r = x + y *)

(*@ function integer_of_int (x : integer) : integer *)
