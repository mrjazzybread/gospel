(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(*@ axiom test : exists . true *)
(* {gospel_expected|
[1] File "invalid_quant3.mli", line 11, characters 24-25:
    11 | (*@ axiom test : exists . true *)
                                 ^
    Error: Syntax error
    
|gospel_expected} *)
