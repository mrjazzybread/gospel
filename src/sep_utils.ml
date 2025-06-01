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
open Tast
module Env = Ident.IdTable
module Set = Set.Make (String)

type namespace = psymbol Env.t

let empty_env () : namespace =
  let env = Env.create 100 in
  let val_ps =
    { ps_name = Constants.val_lens_id; ps_args = []; ps_persistent = true }
  in
  let () = Env.add env Constants.val_lens_id.id_tag val_ps in
  env

let get_pred ns = function
  | Id_uast.Lidapp linfo ->
      let id = Uast_utils.leaf linfo.lid in
      Env.find ns id.id_tag
  | _ -> assert false

let map_pred ns lens =
  let ps =
    {
      ps_name = lens.lid;
      ps_args = [ lens.locaml; lens.lmodel ];
      ps_persistent = lens.lpersistent;
    }
  in
  Env.add ns lens.lid.id_tag ps

let change_id map id = Ident.mk_id (map id.id_str) ~loc:id.id_loc
