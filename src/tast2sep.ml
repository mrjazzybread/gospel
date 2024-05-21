open Tast
open Sep_ast
open Symbols
open Ttypes
open Sep_utilis
open Tterm

type value = {
  s_ty : ty; (* spatial type *)
  arg_prog : vsymbol; (* program variable *)
  arg_log : vsymbol; (* logical value *)
  ro : bool; (* Read only flag *)
}

let is_pure_type vs =
  match vs.vs_ty.ty_node with
  | Tyapp (ts, _) -> (
      match ts.ts_rep with Self -> true | Model (mut, _) -> not mut)
  | _ -> true

let rec map_term ns is_old t =
  let f t = map_term ns is_old t in
  let map_node =
    match t.t_node with
    | Tvar v when is_present ns v.vs_name ->
        Tvar (get_id ns is_old v.vs_name.id_str)
    | Tlet (v, t1, t2) -> Tlet (v, f t1, f t2)
    | Told t -> (map_term ns true t).t_node
    | Tcase (t, l) ->
        Tcase (f t, List.map (fun (p, c, t) -> (p, Option.map f c, f t)) l)
    | Tapp (ls, l) -> Tapp (ls, List.map f l)
    | Tif (t1, t2, t3) -> Tif (f t1, f t2, f t3)
    | Tquant (q, l, t) -> Tquant (q, l, f t)
    | Tbinop (b, t1, t2) -> Tbinop (b, f t1, f t2)
    | Tnot t -> Tnot (f t)
    | Tfield (t, n) -> Tfield (f t, n)
    | _ -> t.t_node
  in
  { t with t_node = map_node }

and val_description ns des =
  match des.vd_spec with
  | None -> assert false (* TODO *)
  | Some spec ->
     let ns = ref ns in
     let to_cfml_arg lb =
       let arg_vs = lb.lb_vs in 
       let arg_id = change_id mk_prog arg_vs.vs_name in                
       let arg_prog_ty = if is_pure_type arg_vs then arg_vs.vs_ty else ty_loc in
       { vs_name = arg_id; vs_ty = arg_prog_ty }
     in
     let cfml_args = List.map (fun x -> if x.lb_label = Lunit then None else Some (to_cfml_arg x)) spec.sp_args in 
     let lifted_args is_old =
       List.filter_map (fun arg ->
           let arg_vs = arg.lb_vs in
           if arg.lb_label = Lunit then None else
             match if is_old then arg.lb_consumes else arg.lb_produces with
             | None -> None
             | Some (s_ty, l_ty) ->
                let ro = not arg.lb_modified in
                let arg_prog = to_cfml_arg arg in 
                let arg_log, ns' =
                  map_id !ns (is_old || ro) arg_vs l_ty
                in
                let () = ns := ns' in
                Some { s_ty; arg_prog; ro; arg_log })
     in
     let mk_lift = function
       | { s_ty; arg_prog; arg_log; _ } ->
          let pred = get_pred !ns s_ty in
          Some (App (pred, [ arg_prog; arg_log ]))
     in
     let lifts = List.filter_map mk_lift in
     let args = lifted_args true spec.sp_args in
     let pre = List.map (fun t -> Pure (map_term !ns true t)) spec.sp_pre in
     let triple_pre = Star (lifts args @ pre) in
     let updates = lifted_args false spec.sp_args in
     let rets = lifted_args false spec.sp_ret in
     let post = List.map (fun t -> Pure (map_term !ns false t)) spec.sp_post in
     let post_cond = Star (lifts (updates @ rets) @ post) in
     let mk_updates = function
       | { arg_log; ro; _ } -> if ro then None else Some arg_log
     in
     let updated_vars = List.filter_map mk_updates (updates @ rets) in
     let triple_post =
       if updated_vars = [] then post_cond else Exists (updated_vars, post_cond)
     in
     Triple
       {
         triple_name = des.vd_name;
         triple_vars =
           List.concat_map
             (function { arg_prog; arg_log; _ } -> [ arg_prog; arg_log ])
             args;
         triple_args = cfml_args;
         triple_rets =
           List.map (function { arg_prog; _ } -> arg_prog) rets;
         triple_pre;
         triple_type = des.vd_type;
         triple_post;
       }

and type_declaration t =
  let id = t.td_ts.ts_ident in
  let self_type =
    {
      ty_node =
        Tyapp
          (t.td_ts, List.map (fun (x, _) -> { ty_node = Tyvar x }) t.td_params);
    }
  in
  let pred_field, mut =
    let arg = Ident.create ~loc:Location.none "model" in
    let model_to_arg model =
      let ty, mut =
        match model with
        | Tast.Self -> (self_type, false)
        | Default (mut, ty) -> (ty, mut)
        | _ -> assert false
      in
      let field = { vs_name = arg; vs_ty = ty } in
      (field, mut)
    in
    match t.td_spec with
    | Some s -> model_to_arg s.ty_model
    | None -> ({ vs_name = arg; vs_ty = self_type }, false)
  in
  let new_id = ty_ident self_type |> change_id get_rep_pred in
  let ty_var_list = List.map fst t.td_params in
  if mut then
    [ Pred (new_id, [ { vs_name = id; vs_ty = ty_loc }; pred_field ]) ]
  else
    [
      Type (id, ty_var_list);
      Pred (new_id, [ { vs_name = id; vs_ty = self_type }; pred_field ]);
    ]

let signature_item_desc ns = function
  | Sig_type (_, l, _) -> List.concat_map (fun t -> type_declaration t) l
  | Sig_val (des, _) -> [ val_description ns des ]
  | Sig_open _ | Sig_use _ -> []
  | Sig_axiom axiom -> [ Axiom axiom ]
  | Sig_function f -> [ Function f ]
  | _ -> assert false

let signature_item ns s =
  List.map
    (fun sep -> { d_node = sep; d_loc = s.sig_loc })
    (signature_item_desc ns s.sig_desc)

open Tmodule

let rec convert_ns tns =
  let preds = Mstr.map create_rep_pred tns.ns_sp in
  {
    sns_pred = preds;
    sns_id = Mstr.empty;
    sns_ns = Mstr.map convert_ns tns.ns_ns;
  }

let process_sigs file =
  let ns = merge_ns file.Tmodule.fl_export ns_with_primitives in
  List.concat_map (signature_item (convert_ns ns)) file.Tmodule.fl_sigs
