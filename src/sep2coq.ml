open Sep_ast
open Coq
open Ttypes
open Symbols
module M = Map.Make(String)

let hprop_name = "hprop"

let type_mapping_list = ["sequence", "list"]

let ty_map = List.fold_left (fun m (k, v) -> M.add k v m) M.empty type_mapping_list

let map_ty v =
  try M.find v ty_map with
  |Not_found -> v

let rec var_of_ty t =
  let coq_var x = coq_var (map_ty x) in 
  match t.ty_node with
  |Tyapp(v, _) when ts_equal v ts_loc -> coq_var v.ts_ident.id_str
  |Tyapp(v, l) ->
    coq_apps (coq_var v.ts_ident.id_str) (List.map var_of_ty l)
  |_ -> assert false

let sep_def d = match d.d_node with
  |Type (id, m, _) ->
    if m then
      None
    else
      Some (Coqtop_param(id.id_str, Coq_type))
  |Pred(id, args) ->
    let types = List.map (fun v -> var_of_ty v.vs_ty) args in
    let t = coq_impls types (Coq_var hprop_name) in 
    Some (Coqtop_param(id.id_str, t))
  |_ -> None

let sep_defs = List.filter_map sep_def
