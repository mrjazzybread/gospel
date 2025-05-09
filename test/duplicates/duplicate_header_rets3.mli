(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

val f : unit -> int * int * int
(*@ let x, y, x = f () in ensures true *)

(* {gospel_expected|
[1] File "duplicate_header_rets3.mli", line 12, characters 14-15:
    12 | (*@ let x, y, x = f () in ensures true *)
                       ^
    Error: The variable x is defined twice in this header
    
|gospel_expected} *)
