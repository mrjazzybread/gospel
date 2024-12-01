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
