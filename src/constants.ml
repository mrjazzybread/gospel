(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

open Uast_utils
open Id_uast

let bool_id = Ident.mk_id "bool"
let prop_id = Ident.mk_id "prop"
let integer_id = Ident.mk_id "integer"
let char_id = Ident.mk_id "char"
let string_id = Ident.mk_id "string"
let float_id = Ident.mk_id "float"
let set_id = Ident.mk_id "set"
let val_id = Ident.mk_id "val"
let unit_id = Ident.mk_id "unit"

let primitive_list =
  let l =
    [
      bool_id;
      prop_id;
      integer_id;
      char_id;
      string_id;
      float_id;
      set_id;
      val_id;
      unit_id;
    ]
  in
  List.map (fun x -> (x.Ident.id_str, x)) l

let ty_prop = PTtyapp (mk_info (Qid prop_id), [])
let ty_val = PTtyapp (mk_info (Qid val_id), [])
let ty_unit = PTtyapp (mk_info (Qid unit_id), [])
