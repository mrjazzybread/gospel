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
let val_lens_id = Ident.mk_id "Val"
let unit_lens_id = Ident.mk_id "Unit"

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

let ty_prop = PTtyapp (mk_info ~mut:false (Qid prop_id), [])
let ty_val = PTtyapp (mk_info ~mut:false (Qid val_id), [])
let ty_unit = PTtyapp (mk_info ~mut:false (Qid unit_id), [])

let lens_unit =
  let lens_desc =
    Lidapp (mk_linfo (Qid unit_lens_id) true ty_unit [] ty_unit [])
  in
  { lens_desc; lens_loc = Location.none }

let lens_val =
  let id = Ident.mk_id "a" in
  let lens_desc =
    Lidapp (mk_linfo (Qid val_lens_id) true (PTtyvar id) [ id ] ty_val [])
  in
  { lens_desc; lens_loc = Location.none }
