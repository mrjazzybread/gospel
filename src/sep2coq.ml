open Sep_ast
open Coq
open Formula
open Ttypes
open Symbols
module M = Map.Make(String)

let to_triple s = "_" ^ s ^ "_spec"


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

exception WIP

let gen_args vs = vs.vs_name.id_str, var_of_ty vs.vs_ty

let gen_args_opt arg = match arg with
  |None -> begin
      match coq_tt with
      |Coq_var v -> v |_ -> assert false end
         , val_type
  |Some vs -> gen_args vs

let gen_spec triple =
  let args = List.map gen_args_opt triple.triple_args in
  let all_vars = List.map gen_args triple.triple_vars in 
  let dynargs = List.map (fun (x, t) -> coq_dyn_of t (coq_var x)) args in 
  let trm = trm_apps_lifted (coq_var triple.triple_name.id_str) dynargs in
  let pre = hempty in
  let post_body = hempty in
  let post = coq_fun ("__UNUSED__", val_type) post_body in
  let triple = coq_apps_var "CFML.SepLifted.Triple" [trm; pre; post] in
  coq_foralls all_vars triple
  
  

let sep_def d = match d.d_node with
  |Type (id, m, _) ->
    if m then
      []
    else
      [Coqtop_param(id.id_str, Coq_type)]
  |Pred(id, args) ->
    let args = List.rev args in 
    let types = List.map (fun v -> var_of_ty v.vs_ty) args in
    let t = coq_impls types hprop in 
    [Coqtop_param(id.id_str, t)]
  |Triple triple ->
    begin try 
      let fun_def = triple.triple_name.id_str, Formula.func_type in
      let fun_triple = gen_spec triple in
      let triple_name = to_triple triple.triple_name.id_str in 
      coqtop_params [fun_def; triple_name, fun_triple]
    with |_ -> [] end
  |_ -> []

let sep_defs l =
  let cfml = List.map (fun s -> "CFML." ^ s) in 
  let imports = 
      [Coqtop_set_implicit_args ;
      Coqtop_require [ "Coq.ZArith.BinInt"; "TLC.LibLogic"; "TLC.LibRelation"; "TLC.LibInt"; "TLC.LibListZ" ] ;
      Coqtop_require (cfml ["SepBase"; "SepLifted"; "WPLib"; "WPLifted"; "WPRecord"; "WPArray"; "WPBuiltin"; "WPLib" ]);
      (*coqtop_require_unless no_mystd_include*)
      Coqtop_require (cfml [ "Stdlib.Array_ml"; "Stdlib.List_ml"; "Stdlib.Sys_ml" ]) ;
      Coqtop_require_import [ "Coq.ZArith.BinIntDef"; "CFML.Semantics"; "CFML.WPHeader" ];
      Coqtop_custom "Delimit Scope Z_scope with Z.";
        Coqtop_custom "Existing Instance WPHeader.Enc_any | 99."] in
  
  imports @ List.concat_map sep_def l


