(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

exception E
exception E1 of int
exception E2 of int * int
exception E2pair of (int * int)
exception E3 of int list
exception E4 of int * int list
exception E5 of (int -> int)
exception E6 of (int -> float -> bool list)
exception E7 of { x : int }
exception E8 of { x : (int -> float) }
exception E9 of { x : int; y : float }
exception E10 of { x : (int -> int -> float); y : float; z : bool }
exception E11 of char

[@@@gospel "val id : 'a -> 'a"]

(*@ function integer_of_int (x:int): integer *)
(*@ function int_of_integer (x:integer): int *)

val f : 'a -> 'a
(*@ x = f y
    raises E1 z -> integer_of_int z = 1
    raises E1 -> false
    raises E1, E2
    raises E2 -> true
    raises E1 -> true
    raises E2pair -> true
    raises E2pair z -> true
    raises E2pair z -> true
    raises E3 l -> (match l with
                   | [] -> false
                   | z :: zs -> integer_of_int z = 2)
    raises E4 (i,l) -> match l with
                   | [] -> true
                   | z :: zs -> z = i
    raises E5 f -> integer_of_int (f (int_of_integer 3)) = 4
    raises E11 begin
      ensures true
    end
    raises E11 z begin
      ensures true
    end
    raises E4 (i, l) begin
      produces i
      produces l
      ensures true
    end
*)
