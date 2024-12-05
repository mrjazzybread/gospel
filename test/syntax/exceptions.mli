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
(*@ match f y with
    | exception E1 z -> ensures integer_of_int z = 1
    | exception E2pair -> ensures true
    | exception E3 l -> ensures (match l with
                         | [] -> false
                         | z :: zs -> integer_of_int z = 2)
    | exception E4 (i,l) -> ensures (match l with
                            | [] -> true
                            | z :: zs -> z = i)
    | exception E5 f -> ensures integer_of_int (f (int_of_integer 3)) = 4
    | exception E11
    | exception E2 (i, j) ->
      produces i
      produces j
      ensures true
    
*)
