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

val free : unit -> t
(*@ requires y = y
      let y = free () in
      produces y
*)

(* {gospel_expected|
[1] File "not_consumed1.mli", line 15, characters 17-18:
    15 | (*@ requires y = y
                          ^
    Error: Unbound value y
    
|gospel_expected} *)
