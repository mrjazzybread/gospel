open Tast
open Sep_ast
open Symbols
open Ttypes
open Sep_utilis

(** lifts a variable into its logical model using a generated representation predicate.
    @param read_only if the variable we are lifting is read_only. If this is set to true, the resulting term will also be read-only
    @param old this flag is set to false if we are lifting the modified version of the variable in a postcondition. This flag should
      not be false while {!read_only} is set to true*)
let mk_pred_app ~read_only ~old ns arg =
  let ty_name = get_ty_name arg.vs_ty in
  let arg_name = arg.vs_name.id_str in
  let pred_vs = 
    try get_pred ns ty_name with 
    |Not_found -> let p, _ = mk_pred_vs ns ty_name arg.vs_ty in p in  
  let term =  mk_sep_term (App(pred_vs, arg::[get_id ns old arg_name])) in 
  let term = if read_only then mk_sep_term (RO (term)) else term in 
  term

open Tterm    

(** Checks if there is a term vs = old vs *)
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
    lifted model from the representation predicate. 
    @param t the term we want to transform
    @param is_old should be set to false if this term is from a precondition
 *)
let rec map_term t id_map is_old = 
  let f t = map_term t id_map is_old in 
  let map_node = match t.t_node with
    |Tvar v when Mstr.mem v.vs_name.id_str id_map ->
      Tvar (Mstr.find v.vs_name.id_str id_map) 
    |Tlet(v, t1, t2) -> Tlet(v, f t1, f t2)
    |Told t -> (map_term t id_map (fun _ -> true)).t_node
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

and val_description ns des =
  let name = des.vd_name in 
  let get_arg x = match x with |Lnone x |Loptional x |Lnamed x | Lghost x -> x |_-> assert false in
  let args_vsym = List.map get_arg des.vd_args in

  let mk_val_sym is_old v =
    let name = 
      if is_old 
      then mk_val v.vs_name.id_str 
      else mk_update v.vs_name.id_str in 
    let vs = {v with vs_name = Ident.create ~loc:Location.none name} in 
    let vs = map_id ns is_old v.vs_name.id_str vs in  
    vs in 

  let all_args = 
    List.concat_map
      (fun v ->
        let vs_loc = {v with vs_ty=ty_loc} in 
        let vs_val = mk_val_sym true v in
        [vs_loc; vs_val])
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
    List.map (fun arg -> mk_pred_app ~read_only:(is_ro arg) ~old:true ns arg) args_vsym in
  
  let spec_terms f is_old = List.map (fun t -> mk_sep_term (Pure (map_term t ns.id_map is_old))) (spec f) in 
  let pre_terms = spec_terms (fun x -> x.sp_pre) (fun _ -> true) in  
  let modified_val = List.map (fun x -> fst(mk_val_sym false x)) modifies_vs in
  let ret_val = List.concat_map (fun x -> let vs, ho_pred = mk_val_sym true x in ho_pred@[vs]) ret in
  let post_write = 
    List.filter_map 
      (fun arg -> 
        if List.mem arg.vs_name.id_str modifies 
        then Some (mk_pred_app ~read_only:false ~old:false ns arg)
        else None) args_vsym in
  let post_write = post_write@(List.map (fun arg -> mk_pred_app ~read_only:false ~old:true ns arg) ret) in
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

and type_declaration ns t = 
  let id = t.td_ts.ts_ident in
  let self_type = 
    {ty_node = Tyapp(t.td_ts, List.map (fun (x, _) -> {ty_node=Tyvar x}) t.td_params)} in
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
      [pred_fields]
    |None -> [] in 
  let new_id = 
    Ident.create ~attrs:id.id_attrs  ~loc:id.id_loc (get_rep_pred id.id_str) in
  let ty_var_list = List.map fst t.td_params in 
  let model_type = 
    List.map (fun x -> x.vs_ty) pred_fields in 
 in
  [Type(id, ty_var_list); Pred(new_id, {vs_name=id; vs_ty=loc_type}::pred_fields)]


let rec signature_item_desc ns = function 
  |Sig_type(_, l, _) ->
    List.concat_map (fun t -> type_declaration ns t) l
  |Sig_val (des, _) ->
    [val_description ns des]
  |Sig_open _ -> []
  |Sig_axiom axiom -> [Axiom axiom]
  |Sig_function f -> [Function f]
  |_ -> assert false

let signature_item ns s =
  (*FIXME needs to create an updated namespace *)
  List.map (fun sep -> {d_node = sep; d_loc = s.sig_loc}) (signature_item_desc empty_ns s.sig_desc)

open Tmodule

let rec convert_ns tns =
  let create_rep_pred ty =
    match ty.ty_node with
    |Tyapp(sym, _) ->
      let id = sym.ts_ident in 
      let model_type = match sym.spatial with
        |Self -> ty
        |Model m -> m in 
      let new_id = change_id get_rep_pred in
      {
        ls_name = new_id;
        ls_args = [ty, model_type];
        ls_value = None;
        ls_constr = false;
        ls_field = false;
      }
    |_ -> assert false in
  let preds = Mstr.map create_rep_pred tns.ty_ns in
  {sns_pred = preds; sns_ns = Mstr.map convert_ns tns.ns_ns}
  
let process_sigs file =
  List.concat_map
    (signature_item file.Tmodule.fl_export) file.Tmodule.fl_sigs
