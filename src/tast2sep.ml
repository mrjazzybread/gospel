open Tast
open Sep_ast
open Symbols
module Env = Map.Make(String)
open Ttypes

type env = vsymbol Env.t

(** maps OCaml variable names to the names given to their lifted models *)
let id_table : env ref = ref Env.empty
(** same as the previous map, but here we map them to their updated models *)
let updated_id_table: env ref = ref Env.empty
(* maps the name of representation predicates to their {!vsymbols}*)
let pred_table : env ref = ref Env.empty

let map_id is_old id vs =
  let table = if is_old then id_table else updated_id_table in
  table := Env.add  id vs !table

let map_pred id vs =
  pred_table := Env.add id vs !pred_table 

let get_id is_old id =
  let table = if is_old then  id_table else updated_id_table in
  try Env.find id !table with
  |Not_found -> begin print_endline id; raise Not_found end

let get_pred pred_name =
  Env.find pred_name !pred_table

let rep_pred_pref = "@R_"

let get_rep_pred s = rep_pred_pref ^ s
let mk_val s = "@V_" ^ s

let mk_update s =  mk_val (s ^ "'")
let id_of_term t = match t.Tterm.t_node with  |Tterm.Tvar v -> v |_ -> assert false
let get_ty_name ty = match ty.ty_node with
  |Tyapp (t, _) -> t.ts_ident.id_str
  |Tyvar v -> v.tv_name.id_str

let ts_loc = mk_ts
               (Ident.create ~loc:Location.none "loc")
               []
let ts_prop = mk_ts
                (Ident.create ~loc:Location.none "Prop")
                []

let ty_prop = ty_app ts_prop []
let ty_loc = ty_app ts_loc []


let tyvar_suf = "@model"
let mk_ho_model var =  Ident.create ~loc:Location.none (var.tv_name.id_str ^ tyvar_suf)
let mk_ho_rp var = Ident.create ~loc:Location.none (get_rep_pred var.tv_name.id_str)

let tyvar_to_pred var =
  let mk_ty n = {ty_node=n} in
  let ty_model =  {ty_node = Tyvar var} in 
  let ty = Tyapp(ts_arrow, [mk_ty (Tyvar var); ty_model; ty_prop]) in
  {vs_name = mk_ho_rp var;vs_ty = mk_ty ty}



(** creates a {!vs_symbol} for a representation predicate application.
    For now, this function only works for monomorphic types. 
    @param pred_name the name for the predicate
    @param self_type the type that is being lifted*)
let mk_pred_vs pred_name self_type =
  let model_type = self_type in
  let pred_type = 
    {ty_node = 
       Tyapp(ts_arrow, [self_type; model_type; ty_prop])} in
  let id = Ident.create ~loc:Location.none pred_name in 
  let vs = {vs_name = id; vs_ty = pred_type} in 
  let () = map_pred pred_name vs in
  vs

let rec get_ty_vars tbl {ty_node = ty} =
  match ty with
  |Tyvar x -> begin
      let pred = tyvar_to_pred x in
      let pred_name = pred.vs_name.id_str in 
      match Hashtbl.find_opt tbl pred_name  with
      |Some _ -> []
      |None -> let vs =
                 try get_pred pred_name with
                 |Not_found -> tyvar_to_pred x in
               Hashtbl.add tbl pred_name vs; [vs] end
  |Tyapp (_, l) -> List.concat_map (get_ty_vars tbl) l 


(** lifts a variable into its logical model using a generated representation predicate.
    @param read_only if the variable we are lifting is read_only. If this is set to true, the resulting term will also be read-only
    @param old this flag is set to false if we are lifting the modified version of the variable in a postcondition. This flag should
      not be false while {!read_only} is set to true*)
let mk_pred_app ~read_only ~old arg =
  let pred_name = get_rep_pred (get_ty_name arg.vs_ty) in
  let tbl : (string, vsymbol) Hashtbl.t = Hashtbl.create 10 in
  let ho_preds = match arg.vs_ty.ty_node with |Tyvar _ -> [] |_ -> get_ty_vars tbl arg.vs_ty in
  let arg_name = arg.vs_name.id_str in 
  let pred_vs = 
    try get_pred pred_name with 
    |Not_found -> mk_pred_vs pred_name arg.vs_ty in 
  let term =  mk_sep_term (App(pred_vs, arg::ho_preds@[get_id old arg_name])) in 
  let term = if read_only then mk_sep_term (RO (term)) else term in 
  term

open Tterm    


let rec find_eq vs t =
  let f t = find_eq vs t in
  match t.t_node with
  |Tapp(ls, [{t_node=Tvar v1;_}; {t_node=Told {t_node=Tvar v2;_}; _}])
   |Tapp(ls, [{t_node=Told {t_node=Tvar v1;_}; _}; {t_node=Tvar v2;_}])
       when ls = ps_equ ->
    v1.vs_name.id_str = v2.vs_name.id_str && v1.vs_name.id_str = vs.vs_name.id_str
  |Tbinop(Tand, t1, t2) -> f t1 || f t2
  |Tlet(v, _, t) when v.vs_name.id_str <> vs.vs_name.id_str ->
    f t
  |_ -> false

(** Replaces usages of the logical model defined in the GOSPEL specification with the 
    lifted model from the representation predicate. This function is a bit hacky at the moment
    since it assumes that there is only one model named "view".
    @param t the term we want to transform
    @param is_old should be set to false if this term is from a precondition
 *)
let rec map_term t is_old = 
  let f t = map_term t is_old in 
  let map_node = match t.t_node with
    |Tlet(v, t1, t2) -> Tlet(v, f t1, f t2)
    |Told t -> (map_term t (fun _ -> true)).t_node
    |Tcase(t, l) -> 
      Tcase(f t, 
            List.map (fun (p, c, t) -> p, Option.map f c, f t) l)
    |Tapp(ls, l) -> Tapp(ls, List.map f l)
    |Tif(t1, t2, t3) -> Tif(f t1, f t2, f t3)
    |Tquant(q, l, t) -> Tquant(q, l, f t)
    |Tbinop(b, t1, t2) -> Tbinop(b, f t1, f t2)
    |Tnot t -> Tnot (f t)
    |Tfield(t, n) -> Tfield(f t, n)
    |_ -> t.t_node in {t with t_node=map_node}


let rec signature_item_desc s = match s with 
  |Sig_type(_, l, _) -> List.concat_map (fun t -> type_declaration t) l
  |Sig_val (des, _) -> [val_description des]
  |Sig_open _ -> []
  |Sig_axiom axiom -> [Axiom axiom]
  |Sig_function f -> [Function f]
  |_ -> assert false


and val_description des =
  let name = des.vd_name in 
  let get_arg x = match x with |Lnone x |Loptional x |Lnamed x | Lghost x -> x |_-> assert false in
  let args_vsym = List.map get_arg des.vd_args in

  let mk_val_sym is_old v =
    let ho_pred = [] in
    let name = 
      if is_old 
      then mk_val v.vs_name.id_str 
      else mk_update v.vs_name.id_str in 
    let vs = {v with vs_name = Ident.create ~loc:Location.none name} in 
    let () = map_id is_old v.vs_name.id_str vs in  
    vs, ho_pred in 

  let all_args = 
    List.concat_map
      (fun v ->
        let vs_loc = {v with vs_ty=ty_loc} in 
        let vs_val, ho_pred = mk_val_sym true v in
        ho_pred@[vs_loc; vs_val])
      args_vsym in 

  let spec f = Option.fold ~none:[] ~some:f des.vd_spec in 
  let ret = spec (fun spec -> List.map get_arg spec.sp_ret) in
  
  let consumes, produces = spec (fun spec -> spec.sp_cs), spec (fun spec -> spec.sp_prod) in

  let consumes_vs, produces_vs =
    List.map (fun x -> id_of_term x.s_term) consumes, List.map (fun x -> id_of_term x.s_term) produces in

  
  let modifies_vs =
    (spec (fun spec -> List.map id_of_term (List.map (fun x -> x.s_term) spec.sp_wr))) @
      (List.filter (fun v1 -> List.exists (fun v2 -> v1.vs_name.id_str = v2.vs_name.id_str) produces_vs) consumes_vs)
  in
  let ensures = spec (fun spec -> spec.sp_post) in
  let modifies_vs =
    List.filter (fun v -> not (List.exists (find_eq v) ensures)) modifies_vs in 
  
  let modifies = List.map (fun x -> x.vs_name.id_str) modifies_vs in
  
  let is_ro = (fun arg -> not (List.mem arg.vs_name.id_str modifies)) in 
  let pre_rw = 
    List.map (fun arg -> mk_pred_app ~read_only:(is_ro arg) ~old:true arg) args_vsym in
  
  let spec_terms f is_old = List.map (fun t -> mk_sep_term (Pure (map_term t is_old))) (spec f) in 
  let pre_terms = spec_terms (fun x -> x.sp_pre) (fun _ -> true) in  
  let modified_val = List.map (fun x -> fst(mk_val_sym false x)) modifies_vs in
  let ret_val = List.concat_map (fun x -> let vs, ho_pred = mk_val_sym true x in ho_pred@[vs]) ret in
  let post_write = 
    List.filter_map 
      (fun arg -> 
        if List.mem arg.vs_name.id_str modifies 
        then Some (mk_pred_app ~read_only:false ~old:false arg)
        else None) args_vsym in
  let post_write = post_write@(List.map (fun arg -> mk_pred_app ~read_only:false ~old:true arg) ret) in
  let post_terms = spec_terms (fun x -> x.sp_post) is_ro in
  let post_star = mk_sep_term (Star (post_write@post_terms)) in 
  let pre = mk_sep_term (Star (pre_rw@pre_terms)) in 
  let post = mk_sep_term (Exists (modified_val@ret_val, post_star)) in
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
  let self_type = 
    {ty_node = Tyapp(t.td_ts, List.map (fun (x, _) -> {ty_node=Tyvar x}) t.td_params)} in
  let loc_type = ty_loc in 
  let pred_fields = 
    match t.td_spec with
    |Some s ->
      let model_to_arg model = 
        let arg = Ident.create ~loc:Location.none "model" in
        let ty = Option.map fst model in
        let ty = Option.value ty ~default:self_type in
        let field = {vs_name = arg; vs_ty = ty} in
        field in
      let pred_fields = model_to_arg s.ty_fields  in
      let tyvars = List.map (fun (x, _) -> tyvar_to_pred x) t.td_params in
      (tyvars@[pred_fields])
    |None -> [] in 
  let new_id = 
    Ident.create ~attrs:id.id_attrs  ~loc:id.id_loc (get_rep_pred id.id_str) in
  let ty_var_list = List.map fst t.td_params in 
  let model_type = 
    List.map (fun x -> x.vs_ty) pred_fields in 
  let pred_type = 
    {ty_node = 
       Tyapp(ts_arrow, self_type::model_type@[ty_prop])} in 
  let () = map_pred new_id.id_str {vs_name=new_id;vs_ty=pred_type} in
  [Type(id, ty_var_list); Pred(new_id, {vs_name=id; vs_ty=loc_type}::pred_fields)]
let signature_item s = 
  List.map (fun sep -> {d_node = sep; d_loc = s.sig_loc}) (signature_item_desc s.sig_desc)
