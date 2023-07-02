open Tast
open Sep_ast
open Symbols
module Env = Map.Make(String)
open Ttypes

type env = bool Env.t

let env : env ref = ref (Env.empty)

let add f l = env := Env.add f l !env 
let get f = Env.find f !env

let rep_pred_pref = "R_"

let get_rep_pred s = rep_pred_pref ^ s
let mk_update s =  s ^ "'"

let mk_val s = "_V_" ^ s

let id_of_term t = match t.Tterm.t_node with  |Tterm.Tvar v -> v.vs_name.id_str |_ -> assert false

let mk_pred_app ~read_only ~old (arg, t) =
  let pred_name = match t with |Tyapp (t, _) -> get_rep_pred t.ts_ident.id_str |_ -> assert false in 
  let arg_val = mk_val arg in 
  let arg_val = if not old then mk_update arg_val else arg_val in 
  let term =  mk_sep_term (App(pred_name, [arg; arg_val])) in 
  let term = if read_only then mk_sep_term (RO (term)) else term in 
    term

open Tterm    

let map_term t = 
  let rec map_term t is_old = 
    let f t = map_term t is_old in 
    let map_node = match t.t_node with 
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
      |Tfield({t_node=Tvar vs;_}, {ls_name={id_str="view";_};_}) -> 
        if is_old 
          then Tvar {vs with vs_name=Ident.create ~loc:Location.none (mk_val vs.vs_name.id_str)} 
          else Tvar {vs with vs_name=Ident.create ~loc:Location.none (mk_update (mk_val vs.vs_name.id_str))} 
      |Tfield(t, n) -> Tfield(f t, n)
      |_ -> t.t_node in {t with t_node=map_node} in 
  map_term t false

let rec signature_item_desc s = match s with 
|Sig_type(_, l, _) -> List.concat_map (fun t -> type_declaration t) l
|Sig_val (des, _) -> [val_description des]
|Sig_open _ -> []
|_ -> assert false


and val_description des =
  let name = des.vd_name in 
  let get_arg x = match x with |Lnone x -> x |_-> assert false in
  let args_sym = List.map get_arg des.vd_args in
  let args = List.map (fun x -> x.vs_name.id_str, x.vs_ty.ty_node) args_sym in  

  let spec f = Option.fold ~none:[] ~some:f des.vd_spec in 

  let modifies = spec (fun spec -> List.map id_of_term spec.sp_wr)   in   
  let pre_rw = List.map (fun arg -> mk_pred_app ~read_only:(not (List.mem (fst arg) modifies)) ~old:true arg) args in 
  let spec_terms f = List.map (fun t -> mk_sep_term (Pure (map_term t))) (spec f) in 
  let post_write = 
    List.filter_map 
      (fun arg -> if List.mem (fst arg) modifies then Some (mk_pred_app ~read_only:false ~old:false arg) else None) args in 
  let post_terms = spec_terms (fun x -> x.sp_post) in
  let pre_terms = spec_terms (fun x -> x.sp_pre) in  
  let post = mk_sep_term (Star (post_write@post_terms)) in 
  let pre = mk_sep_term (Star (pre_rw@pre_terms)) in 


  Triple {
    triple_name = name; 
    triple_args = args_sym; 
    triple_pre=pre; 
    triple_type = des.vd_type; 
    triple_post = post
    }

and type_declaration t = 
  let pred_fields = 
    match t.td_spec with
    |Some s ->  
      let model_to_arg (sym, _) = 
        let arg = sym.ls_name in
        let ty = match sym.ls_value with |Some t -> t |None -> assert false in 
        let field = {vs_name = arg; vs_ty = ty} in
        field in 
      List.map model_to_arg s.ty_fields
    |None -> [] in 
  let id = t.td_ts.ts_ident in 
  let new_id = Ident.create ~attrs:id.id_attrs  ~loc:id.id_loc (get_rep_pred id.id_str) in
  let self_type = {ty_node=Tyapp({ts_ident=id; ts_args=[];ts_alias=None}, [])} in 
  [Type id; Pred(new_id, {vs_name=id; vs_ty=self_type}::pred_fields)]

  

let signature_item s = 
  List.map (fun sep -> {d_node = sep; d_loc = s.sig_loc}) (signature_item_desc s.sig_desc)