open Tast
open Sep_ast
open Symbols
open Ttypes
open Sep_utilis
open Tterm    

type value =
  |Pure_val of {vs : vsymbol option} 
  |Impure of {s_ty : ty; arg_loc : vsymbol; vs : vsymbol}

let is_pure_type vs =
  match vs.vs_ty.ty_node with
  |Tyapp(ts, _) ->
    begin match ts.ts_rep with
    |Self -> true
    |Model (_, mut) -> not mut end
  |_ -> false

(** Replaces usages of the logical model defined in the GOSPEL specification with the 
    lifted model from the representation predicate. 
    @param t the term we want to transform
    @param is_old should be set to false if this term is from a precondition
 *)
let rec map_term ns is_old t = 
  let f t = map_term ns is_old t in 
  let map_node = match t.t_node with
    |Tvar v when is_present ns v.vs_name ->
      Tvar (get_id ns is_old v.vs_name.id_str)
    |Tlet(v, t1, t2) -> Tlet(v, f t1, f t2)
    |Told t -> (map_term ns true t).t_node
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
  match des.vd_spec with
  |None -> assert false (* TODO *)
  |Some spec ->
    let ns = ref ns in
    let lifted_args is_old =
      List.filter_map
        (fun arg ->
          match arg.arg_vs with
          |None -> Some (Pure_val {vs=None})
          |Some arg_vs -> begin
              match if is_old then arg.consumes else arg.produces with
              |None -> None
              |Some (s_ty, l_ty) ->
                let arg_id = change_id arg_vs.vs_name mk_loc in 
                let arg_loc_ty = ty_loc arg_vs.vs_ty in
                let arg_loc = {vs_name=arg_id; vs_ty = arg_loc_ty} in
                let vs, ns' = map_id !ns (is_old || arg.read_only) arg_vs l_ty in
                let () = ns := ns' in
                if is_pure_type arg_vs then
                  Some (Pure_val {vs=Some vs})
                else
                  Some (Impure {s_ty; arg_loc; vs}) end) in
    let mk_lift = function
      |Pure_val _  -> None
      |Impure {s_ty; arg_loc; vs} ->
        let pred = get_pred !ns s_ty in
        Some (App (pred, [arg_loc; vs])) in 
    let lifts = List.filter_map mk_lift in
    let args = lifted_args true spec.sp_args in
    let pre = List.map (fun t -> Pure (map_term !ns true t)) spec.sp_pre in
    let triple_pre = Star ((lifts args) @ pre) in

    let updates = lifted_args false spec.sp_args in
    let rets = lifted_args false spec.sp_ret in
    let post = List.map (fun t -> Pure (map_term !ns false t)) spec.sp_post in
    let post_cond = Star(lifts (updates @ rets) @ post) in
    let mk_updates = function
      |Pure_val _  -> None
      |Impure {vs;_} -> Some vs in 
    let updated_vars = List.filter_map mk_updates (updates@rets) in 
    let updated_model = 
      if updated_vars = [] then
        post_cond 
      else
        Exists(updated_vars, post_cond) in
    let triple_post =
      if spec.sp_ret = [] then
        updated_model
      else
        let mk_ret r =
          match r with 
          |Pure_val {vs} -> vs
          |Impure {arg_loc;_} -> Some arg_loc in 
        let rets = List.filter_map mk_ret rets in 
        Lambda (rets, updated_model) in
    Triple {
        triple_name = des.vd_name;
        triple_vars =
          List.concat_map
            (function |Pure_val {vs} -> Option.fold vs ~some:(fun vs -> [vs]) ~none:[]
                      |Impure {arg_loc;vs;_} -> [arg_loc;vs]) args;
        triple_args =
          List.map (function |Pure_val {vs} -> vs
                             |Impure {arg_loc;_} -> Some arg_loc) args;
        triple_pre;
        triple_type = des.vd_type;
        triple_post;
      }
      
and type_declaration ns t = 
  let id = t.td_ts.ts_ident in
  let self_type = 
    {ty_node = Tyapp(t.td_ts, List.map (fun (x, _) -> {ty_node=Tyvar x}) t.td_params)} in
  let pred_field, mut = 
    let arg = Ident.create ~loc:Location.none "model" in
    let model_to_arg model = 
      let ty = Option.map fst model in
      let ty = Option.value ty ~default:self_type in
      let field = {vs_name = arg; vs_ty = ty} in
      field, Option.fold ~none:false ~some:snd model in
    match t.td_spec with
    |Some s ->
      model_to_arg s.ty_fields
    |None -> {vs_name = arg; vs_ty = self_type}, false in 
  let new_id = (get_pred ns self_type).ls_name in 
  let ty_var_list = List.map fst t.td_params in 
  [Type(id, mut, ty_var_list);
   Pred(new_id, [{vs_name=id; vs_ty=ty_loc self_type};pred_field])]


let signature_item_desc ns = function 
  |Sig_type(_, l, _) ->
    List.concat_map (fun t -> type_declaration ns t) l
  |Sig_val (des, _) -> print_newline();
    [val_description ns des]
  |Sig_open _ -> []
  |Sig_axiom axiom -> [Axiom axiom]
  |Sig_function f -> [Function f]
  |_ -> assert false

let signature_item ns s =
  List.map (fun sep -> {d_node  =sep; d_loc = s.sig_loc}) (signature_item_desc ns s.sig_desc)

open Tmodule

let rec convert_ns tns =
  let preds = Mstr.map create_rep_pred tns.ns_sp in
  {sns_pred = preds; sns_id = Mstr.empty; sns_ns = Mstr.map convert_ns tns.ns_ns}
  
let process_sigs file =
  let ns = merge_ns file.Tmodule.fl_export ns_with_primitives in 
  List.concat_map
    (signature_item (convert_ns ns)) file.Tmodule.fl_sigs
