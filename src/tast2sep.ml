open Tast
open Sep_ast
open Symbols
module Env = Map.Make(String)
open Ttypes

type env = ty Env.t

(** maps variable identifiers with their respective types*)
let id_table : (string, vsymbol) Hashtbl.t = Hashtbl.create 100
(** maps the names of generated representation predicates to their respective types.
   might not be strictly necessary for our translation, but will make the 
   generated AST easier to manipulate *)
let pred_table : (string, vsymbol) Hashtbl.t = Hashtbl.create 100

(** maps the names of types with their respective logical models*)
let env : env ref = ref (Env.empty)

let add ty l = env := Env.add ty l !env 
let rec get ty =
  match ty.ty_node with
  |Tyvar v -> Env.find v.tv_name.id_str !env
  |Tyapp(v, l) ->
    let v_map = try Env.find v.ts_ident.id_str !env with
                |Not_found -> ty in
    match v_map.ty_node with
    |Tyapp(v, _) -> {ty_node = Tyapp(v, List.map get l)}
    |_ -> assert false

let rep_pred_pref = "@R_"

let get_rep_pred s = rep_pred_pref ^ s
let mk_val s = "@V_" ^ s

let mk_update s =  mk_val (s ^ "'")
let id_of_term t = match t.Tterm.t_node with  |Tterm.Tvar v -> v |_ -> assert false
let get_ty_name ty = match ty.ty_node with
  |Tyapp (t, _) -> t.ts_ident.id_str
  |Tyvar v -> v.tv_name.id_str

let ty_prop = ty_app {ts_ident=Ident.create ~loc:Location.none "Prop";
                      ts_args=[];
                      ts_alias=None} []


let tyvar_suf = "@model" 
let mk_ho_model var =  Ident.create ~loc:Location.none (var.tv_name.id_str ^ tyvar_suf)
let mk_ho_rp var = Ident.create ~loc:Location.none (get_rep_pred var.tv_name.id_str)
    
let tyvar_to_pred var =
  let mk_ty n = {ty_node=n} in
  let ty_model = get {ty_node = Tyvar var} in 
  let ty = Tyapp(ts_arrow, [mk_ty (Tyvar var); ty_model; ty_prop]) in
    {vs_name = mk_ho_rp var;vs_ty = mk_ty ty}



(** creates a {!vs_symbol} for a representation predicate application.
    For now, this function only works for monomorphic types. 
    @param pred_name the name for the predicate
    @param self_type the type that is being lifted*)
let mk_pred_vs pred_name self_type =
  let model_type = try get self_type with |Not_found -> self_type in
  let pred_type = 
    {ty_node = 
       Tyapp(ts_arrow, [self_type; model_type; ty_prop])} in
  let id = Ident.create ~loc:Location.none pred_name in 
  let vs = {vs_name = id; vs_ty = pred_type} in 
  let () = Hashtbl.add pred_table pred_name vs in
  vs

let rec get_ty_vars tbl {ty_node = ty} =
    match ty with
    |Tyvar x -> begin
        let pred = tyvar_to_pred x in
        let pred_name = pred.vs_name.id_str in 
      match Hashtbl.find_opt tbl pred_name  with
      |Some _ -> []
      |None -> let vs =
                 try Hashtbl.find pred_table pred_name with
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
  let arg_val = if not old then mk_update arg_name else mk_val arg_name in 
  let pred_vs = 
    try Hashtbl.find pred_table pred_name with 
    |Not_found -> mk_pred_vs pred_name arg.vs_ty in 
  let term =  mk_sep_term (App(pred_vs, arg::ho_preds@[Hashtbl.find id_table arg_val])) in 
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
      |Tfield(t, {ls_name={id_str="view";_};_}) ->
        begin match t.t_node with
        |Tvar vs  -> 
        if is_old vs
          then Tvar (Hashtbl.find id_table (mk_val vs.vs_name.id_str))
          else begin
            try Tvar (Hashtbl.find id_table (mk_update vs.vs_name.id_str)) with 
            |Not_found -> Tvar (Hashtbl.find id_table (mk_val vs.vs_name.id_str)) end
        |Told {t_node=Tvar vs;_} -> Tvar (Hashtbl.find id_table (mk_val vs.vs_name.id_str))
        |_ -> assert false end
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
  let keys = List.map fst (List.of_seq (Hashtbl.to_seq pred_table)) in
  let name = des.vd_name in 
  let get_arg x = match x with |Lnone x |Loptional x |Lnamed x | Lghost x -> x |_-> assert false in
  let args_vsym = List.map get_arg des.vd_args in

  (** Creates a {!vs_symbol} that represents the model of the variable {!v}. 
      This variable will have the same type as the model. To find the model we will 
      lookup the {!env} map. After creating the new variable, we will add it to the hastable 
      {!id_table}. Additionally, we also return representation predicates for any newly introduced
      polymorphic type variables. 
      @param is_old should be true if the variable is read only or if we are creating the {vs_symbol}
      for the old version of a modified variable
      @param v the variable. 
      @param the variable with its model's type as well as the representation predicates for its type variables*)
  let mk_val_sym is_old v =
    let ho_pred = get_ty_vars pred_table v.vs_ty in 
    let ty = try get v.vs_ty with |Not_found -> v.vs_ty in
    let name = 
      if is_old 
        then mk_val v.vs_name.id_str 
        else mk_update v.vs_name.id_str in 
    let vs = {vs_name = Ident.create ~loc:Location.none name; vs_ty = ty} in 
    let () = Hashtbl.add id_table name vs in  
    vs, ho_pred in 


  
  let all_args = 
    List.concat_map (fun v -> let vs, ho_pred = mk_val_sym true v in ho_pred@[v; vs]) args_vsym in 

  let spec f = Option.fold ~none:[] ~some:f des.vd_spec in 
  let ret = spec (fun spec -> List.map get_arg spec.sp_ret ) in
  
  let consumes, produces = spec (fun spec -> spec.sp_cs), spec (fun spec -> spec.sp_cs) in
  let consumes_vs, produces_vs =
    List.map (fun x -> id_of_term x.s_term) consumes, List.map (fun x -> id_of_term x.s_term) produces in
  
  let modifies_vs =
    (spec (fun spec -> List.map id_of_term (List.map (fun x -> x.s_term) spec.sp_wr))) @
    (List.filter (fun v1 -> List.exists (fun v2 -> v1.vs_name.id_str = v2.vs_name.id_str) produces_vs) consumes_vs)
  in
  let ensures = spec (fun spec -> spec.sp_post) in
  let modifies_vs = List.filter (fun v -> not (List.exists (find_eq v) ensures)) modifies_vs in
  
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
  let new_keys = List.map fst (List.of_seq (Hashtbl.to_seq pred_table)) in
  let () = List.iter (fun x -> Hashtbl.remove pred_table x) (List.filter (fun x -> not (List.mem x keys)) new_keys) in
    
  Triple {
    triple_name = name; 
    triple_args = all_args; 
    triple_pre= pre; 
    triple_type = des.vd_type; 
    triple_post = post
  }

and type_declaration t = 
  let id = t.td_ts.ts_ident in
  let () =
    List.iter (fun (x, _) ->
        let var = {tv_name = mk_ho_model x} in
        add x.tv_name.id_str {ty_node=Tyvar var}) t.td_params in 
  let pred_fields = 
    match t.td_spec with
    |Some s ->
      let model_to_arg (sym, _) = 
        let arg = sym.ls_name in
        let ty = match sym.ls_value with |Some t -> t |None -> assert false in 
        let field = {vs_name = arg; vs_ty = get ty} in
        let () = add id.id_str ty in 
        field in 
      let pred_fields = List.map model_to_arg s.ty_fields in
      let tyvars = List.map (fun (x, _) -> tyvar_to_pred x) t.td_params in
      (tyvars@pred_fields)
    |None -> [] in 
  let new_id = 
      Ident.create ~attrs:id.id_attrs  ~loc:id.id_loc (get_rep_pred id.id_str) in
  let ty_var_list = List.map fst t.td_params in 
  let self_type = 
    {ty_node = Tyapp({ts_ident=id;ts_alias=None;ts_args=[]},
                     List.map (fun (x, _) -> {ty_node=Tyvar x}) t.td_params)} in 
  let model_type = 
    List.map (fun x -> x.vs_ty) pred_fields in 
  let pred_type = 
      {ty_node = 
         Tyapp(ts_arrow, self_type::model_type@[ty_prop])} in 
  let () = Hashtbl.add pred_table new_id.id_str {vs_name=new_id;vs_ty=pred_type} in
    [Type(id, ty_var_list); Pred(new_id, {vs_name=id; vs_ty=self_type}::pred_fields)]
let signature_item s = 
  List.map (fun sep -> {d_node = sep; d_loc = s.sig_loc}) (signature_item_desc s.sig_desc)
