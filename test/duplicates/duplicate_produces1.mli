(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

type t
(*@ mutable *)

val f : t -> t
(*@ consumes x
    let y = f x in
      produces x
      produces x
 *)
(* {gospel_expected|
[1] File "duplicate_produces1.mli", line 18, characters 15-16:
    18 |       produces x
                        ^
    Error: The variable x is listed as produced twice
    
|gospel_expected} *)
