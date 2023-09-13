open Tast
open Sep_ast
open Symbols
module Env = Map.Make(String)
open Ttypes

type env = ty Env.t
let id_table : (string, vsymbol) Hashtbl.t = Hashtbl.create 100
let pred_table : (string, vsymbol) Hashtbl.t = Hashtbl.create 100

let env : env ref = ref (Env.empty)

let add f l = env := Env.add f l !env 
let get f = Env.find f !env

let rep_pred_pref = "_R_"

let get_rep_pred s = rep_pred_pref ^ s
let mk_val s = "_V_" ^ s

let mk_update s =  mk_val (s ^ "'")
let id_of_term t = match t.Tterm.t_node with  |Tterm.Tvar v -> v |_ -> assert false

let mk_pred_vs pred_name self_type = 
  let pred_type = 
    {ty_node = 
      Tyapp(ts_arrow, [self_type; self_type; ty_bool])} in 
  let id = Ident.create ~loc:Location.none pred_name in 
  let vs = {vs_name = id; vs_ty = pred_type} in 
  let () = Hashtbl.add pred_table pred_name vs in
  vs

let mk_pred_app ~read_only ~old arg =
  let pred_name = 
    match arg.vs_ty.ty_node with 
    |Tyapp (t, _) -> get_rep_pred t.ts_ident.id_str 
    |_ -> assert false in 
  let arg_name = arg.vs_name.id_str in 
  let arg_val = if not old then mk_update arg_name else mk_val arg_name in 
  let pred_vs = 
    try Hashtbl.find pred_table pred_name with 
    |Not_found -> mk_pred_vs pred_name arg.vs_ty in 
  let term =  mk_sep_term (App(pred_vs, [arg; Hashtbl.find id_table arg_val])) in 
  let term = if read_only then mk_sep_term (RO (term)) else term in 
    term

open Tterm    

let map_term t is_old = 
  let rec map_term t is_old = 
    let f t = map_term t is_old in 
    let map_node = match t.t_node with 
      |Tfield({t_node=Tvar vs;_}, {ls_name={id_str="view";_};_}) -> 
        if is_old
          then Tvar (Hashtbl.find id_table (mk_val vs.vs_name.id_str))
          else begin
            try Tvar (Hashtbl.find id_table (mk_update vs.vs_name.id_str)) with 
            |Not_found -> Tvar (Hashtbl.find id_table (mk_val vs.vs_name.id_str)) end
      |Tlet(v, t1, t2) -> Tlet(v, f t1, f t2)
      |Told t -> (map_term t true).t_node
      |Tcase(t, l) -> 
        Tcase(f t, 
          List.map (fun (p, c, t) -> p, Option.map f c, f t) l)
      |Tapp(ls, l) -> Tapp(ls, List.map f l)
      |Tif(t1, t2, t3) -> Tif(f t1, f t2, f t3)
      |Tquant(q, l, t) -> Tquant(q, l, f t)
      |Tbinop(b, t1, t2) -> Tbinop(b, f t1, f t2)
      |Tnot t -> Tnot (f t)
      |Tfield(t, n) -> Tfield(f t, n)
      |_ -> t.t_node in {t with t_node=map_node} in 
  map_term t is_old

let rec signature_item_desc s = match s with 
|Sig_type(_, l, _) -> List.concat_map (fun t -> type_declaration t) l
|Sig_val (des, _) -> [val_description des]
|Sig_open _ -> []
|Sig_axiom axiom -> [Axiom axiom]
|Sig_function f -> [Function f]
|_ -> assert false


and val_description des =
  let name = des.vd_name in 
  let get_arg x = match x with |Lnone x -> x |_-> assert false in
  let args_vsym = List.map get_arg des.vd_args in

  let mk_val_sym is_old v = 
    let type_name = 
      match v.vs_ty.ty_node with 
      |Tyapp(v, _) -> v.ts_ident.id_str |_ -> assert false in 
    let ty = try get type_name with |Not_found -> v.vs_ty in
    let name = 
      if is_old 
        then mk_val v.vs_name.id_str 
        else mk_update v.vs_name.id_str in 
    let vs = {vs_name = Ident.create ~loc:Location.none name; vs_ty = ty} in 
    let () = Hashtbl.add id_table name vs in  
    vs in 
  let all_args = 
    List.concat_map (fun v -> [v; mk_val_sym true v]) args_vsym in 

  let spec f = Option.fold ~none:[] ~some:f des.vd_spec in 
  let ret = spec (fun spec -> List.map get_arg  spec.sp_ret ) in
  
  let modifies_vs = spec (fun spec -> List.map id_of_term spec.sp_wr) in
  let modifies = List.map (fun x -> x.vs_name.id_str) modifies_vs in 
  let pre_rw = 
    List.map (fun arg -> mk_pred_app ~read_only:(not (List.mem arg.vs_name.id_str modifies)) ~old:true arg) args_vsym in 
  let spec_terms f is_old = List.map (fun t -> mk_sep_term (Pure (map_term t is_old))) (spec f) in 
  let pre_terms = spec_terms (fun x -> x.sp_pre) true in  
  let modified_val = List.map (mk_val_sym false) modifies_vs in
  let post_write = 
    List.filter_map 
      (fun arg -> 
        if List.mem arg.vs_name.id_str modifies 
          then Some (mk_pred_app ~read_only:false ~old:false arg) 
          else None) args_vsym in 
  let post_terms = spec_terms (fun x -> x.sp_post) false in
  let post_star = mk_sep_term (Star (post_write@post_terms)) in 
  let pre = mk_sep_term (Star (pre_rw@pre_terms)) in 
  let post = mk_sep_term (Exists (modified_val, post_star)) in
  let () = print_endline "wow" in 
  let post = mk_sep_term (Lambda (ret, post)) in 
  

  
  Triple {
    triple_name = name; 
    triple_args = all_args; 
    triple_pre= pre; 
    triple_type = des.vd_type; 
    triple_post = post
  }

and type_declaration t = 
  let id = t.td_ts.ts_ident in 
  let pred_fields = 
    match t.td_spec with
    |Some s ->  
      let model_to_arg (sym, _) = 
        let arg = sym.ls_name in
        let ty = match sym.ls_value with |Some t -> t |None -> assert false in 
        let field = {vs_name = arg; vs_ty = ty} in
        let () = add id.id_str ty in 
        field in 
      List.map model_to_arg s.ty_fields
    |None -> [] in 
    let new_id = 
      Ident.create ~attrs:id.id_attrs  ~loc:id.id_loc (get_rep_pred id.id_str) in
    let self_type = 
      {ty_node = Tyapp({ts_ident=id;ts_alias=None;ts_args=[]}, [])} in 
    let model_type = 
      match pred_fields 
      with 
      |[] -> self_type 
      |[x] -> x.vs_ty 
      |_ -> assert false in
    
    let pred_type = 
      {ty_node = 
        Tyapp(ts_arrow, [self_type; model_type; ty_bool])} in 
    let () = Hashtbl.add pred_table new_id.id_str {vs_name=new_id;vs_ty=pred_type} in 
    let self_type = {ty_node=Tyapp({ts_ident=id; ts_args=[];ts_alias=None}, [])} in 
    [Type id; Pred(new_id, {vs_name=id; vs_ty=self_type}::pred_fields)]

  

let signature_item s = 
  List.map (fun sep -> {d_node = sep; d_loc = s.sig_loc}) (signature_item_desc s.sig_desc)