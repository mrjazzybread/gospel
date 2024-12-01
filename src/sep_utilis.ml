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
open Tterm
open Symbols
open Ident
open Sast
module Mstr = Tmodule.Mstr

type namespace = psymbol Mstr.t
type env = vsymbol Mstr.t

let empty_module = Mstr.empty
let empty_env = Mstr.empty
let rep_pred = String.capitalize_ascii

let get_pred ns ty =
  match ty.ty_node with
  | Tyapp (ts, _) ->
      let nm = rep_pred ts.ts_ident.id_str in
      Mstr.find_opt nm ns
  | _ -> None

let map_pred ns ps_name ps_args =
  let ps = { ps_name; ps_args } in
  Mstr.add ps_name.id_str ps ns

let change_id map id =
  Ident.create ~attrs:id.id_attrs ~loc:id.id_loc (map id.id_str)

let match_field f =
  match f with
  | Field_symbol l -> (l.ls_name, l.ls_args, l.ls_value)
  | _ -> assert false

let update_var s = "_" ^ s ^ "_"
let prog_var s = "_prog_" ^ s

let is_present env id =
  let str = id.id_str in
  Mstr.mem str env

let map_id env is_old vs ty =
  let id =
    if (not is_old) && is_present env vs.vs_name then
      change_id update_var vs.vs_name
    else vs.vs_name
  in
  let val_vs = { vs_name = id; vs_ty = ty } in
  let env' = Mstr.add id.id_str val_vs env in
  (val_vs, env')

let get_id env id =
  let name = update_var id in
  try Mstr.find name env with Not_found -> Mstr.find id env

let to_prog_type ty =
  match ty.ty_node with
  | Tyapp (ts, l) -> (
      match snd ts.ts_model with
      | Fields ->
          {
            ty_node =
              Tyapp ({ ts with ts_ident = change_id (( ^ ) "_") ts.ts_ident }, l);
          }
      | _ -> ty)
  | _ -> ty

let unit_vs =
  { vs_name = Ident.create ~loc:Location.none "()"; vs_ty = ty_unit }

let is_pure_type vs =
  match vs.vs_ty.ty_node with
  | Tyapp (ts, _) -> not (fst ts.ts_model)
  | _ -> true

(** Set of polymorphic type variables *)
module Tv_set = Set.Make (struct
  type t = tvsymbol

  let compare t1 t2 = Ttypes.Tvar.compare t1 t2
end)

(** Union of a list of sets*)
let union_all = List.fold_left Tv_set.union Tv_set.empty

let rec get_ty_poly ty =
  match ty.ty_node with
  | Tyvar v -> Tv_set.singleton v
  | Tyapp (_, l) -> union_all (List.map get_ty_poly l)

(** Returns the polymorphic variables in a list of variables*)
let get_vs_poly args = union_all (List.map (fun x -> get_ty_poly x.vs_ty) args)

(** Returns the polymorphic variables in a Gospel term*)
let rec get_term_poly t : Tv_set.t =
  let poly = get_ty_poly t.t_ty in
  Tv_set.union poly
    (match t.t_node with
    | Tapp (_, l) -> union_all (List.map get_term_poly l)
    | Tif (g, t1, t2) ->
        union_all [ get_term_poly g; get_term_poly t1; get_term_poly t2 ]
    | Tlet (_, t1, t2) -> Tv_set.union (get_term_poly t1) (get_term_poly t2)
    | Tcase (t1, l) ->
        Tv_set.union (get_term_poly t1)
          (union_all (List.map (fun (_, _, t) -> get_term_poly t) l))
    | Tquant (_, l, t) ->
        Tv_set.union
          (get_vs_poly (List.map (fun x -> x.bind_vs) l))
          (get_term_poly t)
    | Tlambda (_, t) -> get_term_poly t
    | Tbinop (_, t1, t2) -> Tv_set.union (get_term_poly t1) (get_term_poly t2)
    | Tfield (t, _) | Tnot t | Told t -> get_term_poly t
    | _ -> Tv_set.empty)

let function_poly f =
  let ty =
    match f.Tast.fun_ls with
    | Function_symbol f -> f.ls_value
    | _ -> assert false
  in
  let s =
    Tv_set.union
      (Option.fold f.Tast.fun_def ~some:get_term_poly ~none:Tv_set.empty)
      (get_vs_poly f.fun_params)
    |> Tv_set.union (get_ty_poly ty)
  in
  Tv_set.elements s

let ty_poly tl =
  let sl = List.map get_ty_poly tl in
  Tv_set.elements (union_all sl)

(* Functions for inlining existential variables *)

let is_var v1 t =
  match t.t_node with
  | Tvar v2 -> Ident.equal v1.vs_name v2.vs_name
  | _ -> false

let check_term v t =
  match t.t_node with
  | Tapp (f, [ t1; t2 ]) when ls_equal f ps_equ ->
      if is_var v t1 then Some t2 else if is_var v t2 then Some t1 else None
  | Tbinop (Tiff, t1, t2) ->
      if is_var v t1 then Some t2 else if is_var v t2 then Some t1 else None
  | _ -> None

let rec map_tvars changed tbl t =
  let f = map_tvars changed tbl in
  let t_node =
    match t.t_node with
    | Tvar v when Hashtbl.mem tbl v ->
        let () = changed := true in
        Hashtbl.find tbl v
    | Tapp (x, l) -> Tapp (x, List.map f l)
    | Tif (t1, t2, t3) -> Tif (f t1, f t2, f t3)
    | Tlet (x, t1, t2) -> Tlet (x, f t1, f t2)
    | Tcase (t, l) ->
        Tcase (f t, List.map (fun (x, t1, t2) -> (x, Option.map f t1, f t2)) l)
    | Tquant (x, y, t) -> Tquant (x, y, f t)
    | Tlambda (x, t) -> Tlambda (x, f t)
    | Tbinop (b, t1, t2) -> Tbinop (b, f t1, f t2)
    | Tnot t -> Tnot (f t)
    | Told t -> Told (f t)
    | _ -> t.t_node
  in
  { t with t_node }

let rec map_sep_terms tbl t =
  let changed = ref false in
  let rec t_map = function
    | Lift (v, l) ->
        let l = List.map (map_tvars changed tbl) l in
        Lift (v, l)
    | Pure t -> Pure (map_tvars changed tbl t)
    | Wand (t, l) -> Wand (List.map t_map t, List.map t_map l)
    | Quant (q, l, s) -> Quant (q, l, List.map t_map s)
  in
  if !changed then map_sep_terms tbl (t_map t) else t_map t

let inline (vl, tl) =
  let tbl = Hashtbl.create 10 in
  let rec inner_loop v = function
    | Pure gt :: xs ->
        let t = check_term v gt in
        if Option.is_some t then (t, xs)
        else
          let t, l = inner_loop v xs in
          (t, Pure gt :: l)
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
            let () = Hashtbl.add tbl x t.t_node in
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
