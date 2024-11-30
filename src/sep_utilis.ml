(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

open Symbols
open Ident

let change_id map id =
  Ident.create ~attrs:id.id_attrs ~loc:id.id_loc (map id.id_str)

let rep_pred = String.capitalize_ascii

let match_field f =
  match f with
  | Field_symbol l -> (l.ls_name, l.ls_args, l.ls_value)
  | _ -> assert false
