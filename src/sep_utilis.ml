(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

open Ttypes
open Symbols
open Ident
open Sast
module Mstr = Tmodule.Mstr
module Set = Set.Make (String)

type namespace = psymbol Mstr.t

let empty_module = Mstr.empty

let get_pred ns ty =
  match ty.ty_node with
  | Tyapp (ts, _) -> (
      try Some (Mstr.find ts.ts_ident.id_str ns) with Not_found -> None)
  | _ -> None

let map_pred ns ps_name ps_args =
  let ps = { ps_name; ps_args } in
  Mstr.add ps_name.id_str ps ns

let change_id map id =
  Ident.create ~attrs:id.id_attrs ~loc:id.id_loc (map id.id_str)

let rep_pred = String.capitalize_ascii

let match_field f =
  match f with
  | Field_symbol l -> (l.ls_name, l.ls_args, l.ls_value)
  | _ -> assert false
