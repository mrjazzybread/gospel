(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

open Ident
open Sast
module Env = Ident.IdTable
module Set = Set.Make (String)

type namespace = psymbol Env.t

let empty_env () : namespace = Env.create 100

let get_pred ns ty =
  match ty with
  | Id_uast.PTtyapp (ts, _) ->
      let id = Uast_utils.leaf ts.app_qid in
      Env.find_opt ns id.id_tag
  | _ -> None

let map_pred ns tid ps_name ps_args =
  let ps = { ps_name; ps_args } in
  Env.add ns tid ps

let change_id map id = Ident.mk_id (map id.id_str) ~loc:id.id_loc
let rep_pred = String.capitalize_ascii
