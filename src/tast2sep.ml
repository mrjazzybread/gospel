open Tast
open Sep_ast
open Symbols
open Ttypes
open Sep_utilis
open Tterm    

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
          match if is_old then arg.consumes else arg.produces with
          |None -> None
          |Some (s_ty, l_ty) ->
            let arg_vs = Option.get arg.arg_vs in
            let arg_loc_ty = ty_loc arg_vs.vs_ty in
            let arg_loc = {arg_vs with vs_ty = arg_loc_ty} in
            let vs, ns' =
              if (not is_old) && arg.read_only then
                get_id !ns true arg_vs.vs_name.id_str, !ns
              else
                map_id !ns arg_vs l_ty in
            let () = ns := ns' in
            Some (s_ty, arg_loc, vs)) in
    let lifts args =
      List.map
        (fun (s_ty, arg_loc, vs) ->
          let pred = get_pred !ns s_ty in
          App (pred, [arg_loc; vs]))
        args in
    let args = lifted_args true spec.sp_args in
    let pre = List.map (fun t -> Pure (map_term !ns true t)) spec.sp_pre in
    let triple_pre = Star ((lifts args) @ pre) in

    let updates = lifted_args false spec.sp_args @ lifted_args false spec.sp_ret in
    let post = List.map (fun t -> Pure (map_term !ns false t)) spec.sp_post in
    let post_cond = Star((lifts updates) @ post) in
    let updated_vars =
      List.filter_map (fun (_, _, vs) ->
          if List.exists (fun (_, _, vs2) -> vs2 = vs) args
          then None else Some vs) updates in 
    let updated_model = 
      if updated_vars = [] then
        post_cond 
      else
        Exists(updated_vars, post_cond) in
    let triple_post =
      if spec.sp_ret = [] then
        updated_model
      else
        let eq vs ret =
          let ret_vs = Option.get ret.arg_vs in
          ret_vs.vs_name.id_str = vs.vs_name.id_str in 
        let rets =
          List.filter_map
            (fun (_, ret_loc, _) ->
              if List.exists (fun r -> eq ret_loc r) spec.sp_ret then
                Some ret_loc
              else
                None
            ) updates in 
        Lambda (rets, updated_model) in
    Triple {
        triple_name = des.vd_name;
        triple_args = List.concat_map (fun (_, x, y) -> [x; y]) args;
        triple_pre;
        triple_type = des.vd_type;
        triple_post;
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
  let new_id = (get_pred ns self_type).ls_name in 
  let ty_var_list = List.map fst t.td_params in 
  [Type(id, ty_var_list); Pred(new_id, {vs_name=id; vs_ty=ty_loc self_type}::pred_fields)]


let signature_item_desc ns = function 
  |Sig_type(_, l, _) ->
    List.concat_map (fun t -> type_declaration ns t) l
  |Sig_val (des, _) ->
    [val_description ns des]
  |Sig_open _ -> []
  |Sig_axiom axiom -> [Axiom axiom]
  |Sig_function f -> [Function f]
  |_ -> assert false

let signature_item ns s =
  List.map (fun sep -> {d_node = sep; d_loc = s.sig_loc}) (signature_item_desc ns s.sig_desc)

open Tmodule

let rec convert_ns tns =
  let preds = Mstr.map create_rep_pred tns.ns_ts in
  {sns_pred = preds; sns_id = Mstr.empty; sns_ns = Mstr.map convert_ns tns.ns_ns}
  
let process_sigs file =
  List.concat_map
    (signature_item (convert_ns file.Tmodule.fl_export)) file.Tmodule.fl_sigs
