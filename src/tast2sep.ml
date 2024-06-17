open Tast
open Symbols
open Ttypes
open Sep_utilis
open Tterm
open Sep_ast

type value = {
  s_ty : ty; (* spatial type *)
  arg_prog : vsymbol; (* program variable *)
  arg_log : vsymbol; (* logical value *)
  ro : bool; (* Read only flag *)
}

(** Module defining the function that inlines existentially quantified
    variables. *)
module Inline : sig
  val inline_def : triple -> triple
end = struct
  let is_var v1 t =
    match t.t_node with
    | Tvar v2 -> Ident.equal v1.vs_name v2.vs_name
    | _ -> false

  let check_term v t =
    match t.t_node with
    | Tapp (_, f, [ t1; t2 ]) when ls_equal f ps_equ ->
        print_endline (Tterm.show_term t1);
        if is_var v t1 then Some t2 else if is_var v t2 then Some t1 else None
    | _ -> None

  let map_sep_terms tbl t =
    match t with
    | App (v, l) ->
        let l =
          List.map
            (fun t ->
              match t.t_node with
              | Tvar v when Hashtbl.mem tbl v -> Hashtbl.find tbl v
              | _ -> t)
            l
        in
        App (v, l)
    | _ -> t

  let inline (vl, tl) =
    let tbl = Hashtbl.create 10 in
    let rec inner_loop v = function
      | Pure gt :: xs ->
          let t = check_term v gt in
          if Option.is_some t then (t, xs)
          else
            let t, l = inner_loop v xs in
            (t, Pure gt :: l)
      | x :: xs ->
          let t, l = inner_loop v xs in
          (t, x :: l)
      | [] -> (None, [])
    in
    let rec loop tl = function
      | x :: xs -> (
          let t, tl = inner_loop x tl in
          match t with
          | Some t ->
              let () = Hashtbl.add tbl x t in
              let xs, tl = loop tl xs in
              (xs, tl)
          | None ->
              let l, tl = loop tl xs in
              (x :: l, tl))
      | [] -> ([], tl)
    in
    let vl, tl = loop tl vl in
    (vl, List.map (map_sep_terms tbl) tl)

  let inline_def t = { t with triple_post = inline t.triple_post }
end

let is_pure_type vs =
  match vs.vs_ty.ty_node with
  | Tyapp (ts, _) -> (
      match ts.ts_rep with Self -> true | Model (mut, _) -> not mut)
  | _ -> true

let rec get_poly ty =
  match ty.ty_node with
  | Tyvar v -> [ v ]
  | Tyapp (_, l) -> List.concat_map get_poly l

let rm_dup l = List.sort_uniq Ttypes.Tvar.compare l
let get_poly_list args = List.concat_map (fun t -> get_poly t) args
let get_vs_poly args = get_poly_list (List.map (fun x -> x.vs_ty) args)

let get_value_poly args =
  get_poly_list (List.map (fun x -> x.arg_log.vs_ty) args) |> rm_dup

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
    | Tapp (qual, ls, l) -> Tapp (qual, ls, List.map f l)
    | Tif (t1, t2, t3) -> Tif (f t1, f t2, f t3)
    | Tquant (q, l, t) -> Tquant (q, l, f t)
    | Tbinop (b, t1, t2) -> Tbinop (b, f t1, f t2)
    | Tnot t -> Tnot (f t)
    | Tfield (t, qual, n) -> Tfield (f t, qual, n)
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
        let arg_prog_ty =
          if is_pure_type arg_vs then arg_vs.vs_ty else ty_loc
        in
        { vs_name = arg_id; vs_ty = arg_prog_ty }
      in
      let cfml_args =
        List.map
          (fun x -> if x.lb_label = Lunit then None else Some (to_cfml_arg x))
          spec.sp_args
      in
      let lifted_args is_old =
        List.filter_map (fun arg ->
            let arg_vs = arg.lb_vs in
            if arg.lb_label = Lunit then None
            else
              match if is_old then arg.lb_consumes else arg.lb_produces with
              | None -> None
              | Some (s_ty, l_ty) ->
                  let ro = not arg.lb_modified in
                  let arg_prog = to_cfml_arg arg in
                  let arg_log, ns' = map_id !ns (is_old || ro) arg_vs l_ty in
                  let () = ns := ns' in
                  Some { s_ty; arg_prog; ro; arg_log })
      in
      let mk_lift = function
        | { s_ty; arg_prog; arg_log; _ } ->
            let pred = get_pred !ns s_ty in
            Some
              (App
                 ( pred,
                   [
                     Tterm_helper.t_var arg_prog Location.none;
                     Tterm_helper.t_var arg_log Location.none;
                   ] ))
      in
      let lifts = List.filter_map mk_lift in
      let args = lifted_args true spec.sp_args in
      let pre = List.map (fun t -> Pure (map_term !ns true t)) spec.sp_pre in
      let triple_pre = lifts args @ pre in
      let updates = lifted_args false spec.sp_args in
      let rets = lifted_args false spec.sp_ret in
      let post = List.map (fun t -> Pure (map_term !ns false t)) spec.sp_post in
      let triple_poly = get_value_poly (args @ rets) in
      let post_cond = lifts (updates @ rets) @ post in
      let mk_updates = function
        | { arg_log; ro; _ } -> if ro then None else Some arg_log
      in
      let updated_vars = List.filter_map mk_updates (updates @ rets) in
      let triple_post = (updated_vars, post_cond) in
      Triple
        (Inline.inline_def
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
             triple_poly;
             triple_type = des.vd_type;
             triple_post;
           })

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
  let arg_ty = if mut then ty_loc else self_type in
  let ty = { type_name = id; type_args = ty_var_list; type_mut = mut } in
  let pred =
    {
      pred_name = new_id;
      pred_args = [ { vs_name = id; vs_ty = arg_ty }; pred_field ];
      pred_poly = ty_var_list;
    }
  in
  [ Type ty; Pred pred ]

let gather_poly t =
  let rec gather_poly t =
    let poly = get_poly t.t_ty in
    poly
    @
    match t.t_node with
    | Tapp (_, _, l) -> List.concat_map gather_poly l
    | Tif (g, t1, t2) -> gather_poly g @ gather_poly t1 @ gather_poly t2
    | Tlet (_, t1, t2) -> gather_poly t1 @ gather_poly t2
    | Tcase (t1, l) ->
        gather_poly t1 @ List.concat_map (fun (_, _, t) -> gather_poly t) l
    | Tquant (_, l, t) -> get_vs_poly l @ gather_poly t
    | Tlambda (_, t) -> gather_poly t
    | Tbinop (_, t1, t2) -> gather_poly t1 @ gather_poly t2
    | Tfield (t, _, _) | Tnot t | Told t -> gather_poly t
    | _ -> []
  in
  gather_poly t

let rec signature_item_desc ns = function
  | Sig_type (_, l, _) -> List.concat_map (fun t -> type_declaration t) l
  | Sig_val (des, _) -> [ val_description ns des ]
  | Sig_axiom axiom -> [ Axiom (rm_dup (gather_poly axiom.ax_term), axiom) ]
  | Sig_function f ->
      let poly =
        Option.fold f.fun_def ~some:gather_poly ~none:[]
        @ get_vs_poly f.fun_params
        @ get_poly f.fun_ls.ls_value
      in
      [ Function (rm_dup poly, f) ]
  | Sig_module m -> (
      match m.md_type.mt_desc with
      | Mod_signature s ->
          let nm = m.md_name in
          let defs = List.concat_map (signature_item ns) s in
          [ Module (nm, defs) ]
      | _ -> assert false)
  | Sig_open _ | Sig_use _ | Sig_extension _ | Sig_attribute _ -> []
  | _ -> assert false

and signature_item ns s =
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
  List.concat_map
    (fun s -> signature_item (convert_ns ns) s)
    file.Tmodule.fl_sigs
