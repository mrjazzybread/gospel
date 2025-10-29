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
    { ps_name = Constants.val_lens_id; ps_args = []; ps_sep = false }
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
      ps_sep = not lens.lpersistent;
    }
  in
  Env.add ns lens.lid.id_tag ps

let change_id map id = Ident.mk_id (map id.id_str) ~loc:id.id_loc

let is_var v1 t =
  match t.t_node with
  | Tvar v2 | Ttyapply (v2, _) -> Ident.equal v1.ts_id (Uast_utils.leaf v2)
  | _ -> false

let rec app_list =
  let open Uast_utils in
  function
  | Tvar v | Ttyapply (v, _) ->
      let id = Uast_utils.leaf v in
      let is_equal_or_iff =
        (id.id_str = "infix =" || id.id_str = "infix <->") && Ident.is_stdlib id
      in
      if is_equal_or_iff then Some [] else None
  | Tapply (t1, t2) ->
      let* l = app_list t1.t_node in
      t2 :: l
  | _ -> None

let check_term v t =
  match app_list t.t_node with
  | Some [ t1; t2 ] ->
      if is_var v t1 then Some t2 else if is_var v t2 then Some t1 else None
  | _ -> None

let rec map_tvars changed tbl t =
  let f = map_tvars changed tbl in
  let t_node =
    match t.t_node with
    | Tvar v when Env.mem tbl (Uast_utils.leaf v).id_tag ->
        let () = changed := true in
        Env.find tbl (Uast_utils.leaf v).id_tag
    | Tapply (t1, t2) -> Tapply (f t1, f t2)
    | Tif (t1, t2, t3) -> Tif (f t1, f t2, f t3)
    | Tlet (x, t1, t2) -> Tlet (x, f t1, f t2)
    | Tquant (x, y, t) -> Tquant (x, y, f t)
    | Tlambda (x, t, pty) -> Tlambda (x, f t, pty)
    | Told t -> Told (f t)
    | _ -> t.t_node
  in
  { t with t_node }

let rec map_sep_terms tbl t =
  let changed = ref false in
  let rec t_map = function
    | Lift (v, t1, t2) ->
        Lift (v, map_tvars changed tbl t1, map_tvars changed tbl t2)
    | Logical t -> Logical (map_tvars changed tbl t)
    | Wand (t, l) -> Wand (List.map t_map t, List.map t_map l)
    | Quant (q, l, s) -> Quant (q, l, List.map t_map s)
  in
  let t = t_map t in
  if !changed then map_sep_terms tbl t else t

let inline (vl, tl) =
  let tbl = Env.create 10 in
  let rec inner_loop v = function
    | Logical gt :: xs ->
        let t = check_term v gt in
        if Option.is_some t then (t, xs)
        else
          let t, l = inner_loop v xs in
          (t, Logical gt :: l)
    | x :: xs ->
        let t, l = inner_loop v xs in
        (t, x :: l)
    | [] -> (None, [])
  in
  let rec loop tl = function
    | x :: xs -> (
        let t, tl = inner_loop x tl in
        match t with
        | Some t ->
            let () = Env.add tbl x.ts_id.id_tag t.t_node in
            let xs, tl = loop tl xs in
            (xs, tl)
        | None ->
            let l, tl = loop tl xs in
            (x :: l, tl))
    | [] -> ([], tl)
  in
  let vl, tl = loop tl vl in
  (vl, List.map (map_sep_terms tbl) tl)

let inline_def t = { t with triple_post = inline t.triple_post }
