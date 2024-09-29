open Tast
open Symbols
open Ttypes
open Sep_utilis
open Tterm
open Sep_ast

type context = { mutable mod_nm : string }

let context = { mod_nm = "" }

type spatial_info = {
  arg_pred : lsymbol; (* Representation Predicate *)
  arg_prog : vsymbol; (* Program variable *)
  ro : bool; (* Read only flag *)
}

type value = {
  arg_log : vsymbol;
  (** Logical value *)
  arg_spatial : spatial_info option;
  (** Info on ownership. None if the value is duplicable *)
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
        if is_var v t1 then Some t2 else if is_var v t2 then Some t1 else None
    | Tbinop (Tiff, t1, t2) ->
        if is_var v t1 then Some t2 else if is_var v t2 then Some t1 else None
    | _ -> None

  let rec map_tvars changed tbl t =
    let f = map_tvars changed tbl in
    let t_node =
      match t.t_node with
      | Tvar v when Hashtbl.mem tbl v ->
          let () = changed := true in
          Hashtbl.find tbl v
      | Tapp (x, y, l) -> Tapp (x, y, List.map f l)
      | Tif (t1, t2, t3) -> Tif (f t1, f t2, f t3)
      | Tlet (x, t1, t2) -> Tlet (x, f t1, f t2)
      | Tcase (t, l) ->
          Tcase (f t, List.map (fun (x, t1, t2) -> (x, Option.map f t1, f t2)) l)
      | Tquant (x, y, t) -> Tquant (x, y, f t)
      | Tlambda (x, t) -> Tlambda (x, f t)
      | Tbinop (b, t1, t2) -> Tbinop (b, f t1, f t2)
      | Tnot t -> Tnot (f t)
      | Told t -> Told (f t)
      | _ -> t.t_node
    in
    { t with t_node }

  let rec map_sep_terms tbl t =
    let changed = ref false in
    let rec t_map = function
      | Lift (v, l) ->
          let l = List.map (map_tvars changed tbl) l in
          Lift (v, l)
      | Pure t -> Pure (map_tvars changed tbl t)
      | Wand (t, l) -> Wand(List.map t_map t, List.map t_map l)
      | Quant (q, l, s) -> Quant (q, l, List.map t_map s)
    in
    if !changed then map_sep_terms tbl (t_map t) else t_map t

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
              let () = Hashtbl.add tbl x t.t_node in
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
      match ts.ts_rep with
      | Self -> true
      | Model (mut, _) | Fields mut -> not mut)
  | _ -> true

let rec get_poly ty =
  match ty.ty_node with
  | Tyvar v -> [ v ]
  | Tyapp (_, l) -> List.concat_map get_poly l

let rm_dup l = List.sort_uniq Ttypes.Tvar.compare l
let get_poly_list args = List.concat_map (fun t -> get_poly t) args
let get_vs_poly args = get_poly_list (List.map (fun x -> x.vs_ty) args)

let unit_vs =
  { vs_name = Ident.create ~loc:Location.none "()"; vs_ty = ty_unit }

let get_value_poly args =
  get_poly_list (List.map (fun x -> x.arg_log.vs_ty) args) |> rm_dup


let mk_lift {arg_spatial; arg_log} = 
  Option.map
    (fun spatial ->
      Lift
        ( spatial.arg_pred,
          [
            Tterm_helper.t_var spatial.arg_prog Location.none;
            Tterm_helper.t_var arg_log Location.none;
    ] ))
    arg_spatial


let to_cfml_arg arg_vs =
  let arg_id = change_id mk_prog arg_vs.vs_name in
  let arg_prog_ty =
    if is_pure_type arg_vs then
      to_prog_type arg_vs.vs_ty
    else ty_loc
  in
  { vs_name = arg_id; vs_ty = arg_prog_ty }


let rec tterm_to_sep ns t =
  let quant_lift binder =
    let pred = get_pred ns binder.bind_spatial in
    let spatial_info =
      Option.map
        (fun pred ->
          {arg_pred = pred;
           arg_prog = to_cfml_arg binder.bind_vs;
           ro = true; }
        ) pred in
    { arg_log = binder.bind_vs;
      arg_spatial=spatial_info } in
  
  match t.t_node with
  |Tquant(q, l, t) ->
    let vlist = List.map quant_lift l in
    let lifts = List.filter_map mk_lift vlist in
    let vsl =
      List.concat_map
        (fun x ->
          (Option.fold
             ~none:[]
             ~some:(fun x -> [x.arg_prog]) x.arg_spatial)
          @[x.arg_log])
        vlist in
    let quant = Quant(q, vsl, tterm_to_sep ns t) in
    begin match lifts with
    |[] -> [quant]
    |_ -> [Wand(lifts, quant :: lifts)]
    end
  |Tbinop(Tand, t1, t2) ->
    tterm_to_sep ns t1 @ tterm_to_sep ns t2
  |_ -> [Pure t] 

let map_term ns is_old t =
  let rec change_vars is_old t =
    let f t = change_vars is_old t in
    let map_node =
      match t.t_node with
      | Tvar v when is_present ns v.vs_name ->
         Tvar (get_id ns is_old v.vs_name.id_str)
      | Tlet (v, t1, t2) -> Tlet (v, f t1, f t2)
      | Told t -> (change_vars true t).t_node
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
    { t with t_node = map_node } in
  change_vars is_old t |> tterm_to_sep ns

let val_description ns des =
  match des.vd_spec with
   | spec ->
      let ns = ref ns in      
      let lifted_args is_old =
        List.filter_map (fun arg ->
            let arg_vs = arg.lb_vs in
            if arg.lb_label = Lunit then
              Some { arg_spatial = None; arg_log = unit_vs }
            else
              match if is_old then arg.lb_consumes else arg.lb_produces with
              | None -> None
              | Some (s_ty, l_ty) ->
                  let ro = not arg.lb_modified in
                  let arg_log, ns' = map_id !ns (is_old || ro) arg_vs l_ty in
                  let () = ns := ns' in
                  let pred = get_pred !ns s_ty in
                  let arg_prog = to_cfml_arg arg.lb_vs in
                  let arg_spatial =
                    Option.map (fun arg_pred -> { arg_pred; arg_prog; ro }) pred
                  in
                  Some { arg_spatial; arg_log })
      in
      
      let lifts = List.filter_map mk_lift in
      let args = lifted_args true spec.sp_args in
      let mk_cfml_arg arg =
        match arg.arg_spatial with Some s -> s.arg_prog | None -> arg.arg_log
      in
      let cfml_args = List.map mk_cfml_arg args in
      let pre = List.concat_map (fun t -> map_term !ns true t) spec.sp_pre in
      let triple_pre = lifts args @ pre in
      let updates = lifted_args false spec.sp_args in
      let rets = lifted_args false spec.sp_ret in
      let post = List.concat_map (fun t -> map_term !ns false t) spec.sp_post in
      let triple_poly = get_value_poly (args @ rets) in
      let post_cond = lifts (updates @ rets) @ post in
      let mk_updates = function
        | { arg_log; arg_spatial } ->
            Option.bind arg_spatial (fun x ->
                if x.ro then None else Some arg_log)
      in

      let updated_vars = List.filter_map mk_updates (updates @ rets) in
      let triple_post = (updated_vars, post_cond) in
      Triple
        (Inline.inline_def
           {
             triple_name = des.vd_name;
             triple_vars =
               List.concat_map
                 (function
                   | { arg_spatial; arg_log } -> (
                       let l =
                         if arg_log.vs_name.id_str = "()" then []
                         else [ arg_log ]
                       in
                       match arg_spatial with
                       | Some s -> s.arg_prog :: l
                       | None -> l))
                 args;
             triple_args = cfml_args;
             triple_rets = List.map mk_cfml_arg rets;
             triple_checks = spec.sp_checks;
             triple_pre;
             triple_poly;
             triple_type = des.vd_type;
             triple_post;
           })

let type_declaration t =
  let ts = t.td_ts in
  let spec = t.td_spec in
  let tvar_list = List.map (fun (x, _) -> { ty_node = Tyvar x }) t.td_params in
  let model_type = match spec with None -> Tast.Self | Some s -> s.ty_model in
  let is_record = match model_type with Tast.Fields _ -> true | _ -> false in
  let is_mutable =
    match model_type with
    | Self -> false
    | Default (mut, _) -> mut
    | Fields l -> List.exists (fun (x, _) -> x) l
  in
  (* If the type has multiple model fields, this is a singleton list
     with a type with all of the appropriate model fields. Otherwise, it
     is empty. *)
  let model_decl =
    match model_type with
    | Tast.Fields l ->
       let fields =
         List.map
           (fun (_, ls) ->
             let id = ls.ls_name in
             let ty = ls.ls_value in
             (id, ty))
           l
       in
       let def = Record fields in
       Some
         (Type
            { type_name = ts.ts_ident; type_args = ts.ts_args; type_def = def })
    | _ -> None
  in

  (* Type declaration *)
  let type_name =
    if is_record then change_id (( ^ ) "_") ts.ts_ident else ts.ts_ident
  in

  let type_decl =
    if not is_mutable then
      Some (Type { type_name; type_args = ts.ts_args; type_def = Abstract })
    else None
  in
  (* Predicate definition *)
  let prog_ts = { ts with ts_ident = type_name } in
  let pred_prog_ty =
    if is_mutable then ty_loc else { ty_node = Tyapp (prog_ts, tvar_list) }
  in

  let prog_vs =
    { vs_name = Ident.create "target" ~loc:Location.none; vs_ty = pred_prog_ty }
  in

  let pred_model_ty =
    match model_type with
    | Self -> pred_prog_ty
    | Fields _ -> { ty_node = Tyapp (ts, tvar_list) }
    | Default (_, t) -> t
  in
  let model_vs =
    { vs_name = Ident.create "model" ~loc:Location.none; vs_ty = pred_model_ty }
  in
  let new_id =
    if ts.ts_ident.id_str = "t" then
      Ident.create ~loc:Location.none context.mod_nm
    else ts.ts_ident |> change_id get_rep_pred
  in

  let pred_def = if not is_mutable && model_type <> Self then None else
                   Some (Pred
                           {
                             pred_name = new_id;
                             pred_args = [ prog_vs; model_vs ];
                             pred_poly = ts.ts_args;
                     })
  in
  let cons x l = match x with None -> l | Some x -> x :: l in
  cons type_decl (cons model_decl (cons pred_def []))

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
    | Tquant (_, l, t) -> get_vs_poly (List.map (fun x-> x.bind_vs) l) @ gather_poly t
    | Tlambda (_, t) -> gather_poly t
    | Tbinop (_, t1, t2) -> gather_poly t1 @ gather_poly t2
    | Tfield (t, _, _) | Tnot t | Told t -> gather_poly t
    | _ -> []
  in
  gather_poly t

let rec signature_item_desc ns = function
  | Sig_type (_, l, _) -> List.concat_map (fun t -> type_declaration t) l
  | Sig_val (des, _) -> [ val_description ns des ]
  | Sig_axiom axiom ->
     let poly = gather_poly axiom.ax_term in
     let axiom =
       {sax_name = axiom.ax_name;
        sax_loc = axiom.ax_loc;
        sax_term = tterm_to_sep ns axiom.ax_term } in
     [
      Axiom (rm_dup poly, axiom)
    ]
  | Sig_function f ->
      let poly =
        Option.fold f.fun_def ~some:gather_poly ~none:[]
        @ get_vs_poly f.fun_params
        @ get_poly f.fun_ls.ls_value
      in
      [ Function (rm_dup poly, f) ]
  | Sig_module m -> (
      let () = context.mod_nm <- m.md_name.id_str in
      match m.md_type.mt_desc with
      | Mod_signature s ->
          let nm = m.md_name in
          let defs = List.concat_map (signature_item ns) s in
          [ Module (nm, defs) ]
      | _ -> assert false)
  | Sig_open (op, _) -> [ Import op.opn_id ]
  | Sig_use _ | Sig_extension _ | Sig_attribute _ -> []
  | _ -> assert false

and signature_item ns s =
  List.map
    (fun sep -> { d_node = sep; d_loc = s.sig_loc })
    (signature_item_desc ns s.sig_desc)

open Tmodule

let rec convert_ns tns =
  let preds = Mstr.filter_map create_rep_pred tns.ns_sp in
  {
    sns_pred = preds;
    sns_id = Mstr.empty;
    sns_ns = Mstr.map convert_ns tns.ns_ns;
  }

let process_sigs file =
  let () = context.mod_nm <- file.fl_nm.id_str in
  let ns = merge_ns file.Tmodule.fl_export ns_with_primitives in
  List.concat_map
    (fun s -> signature_item (convert_ns ns) s)
    file.Tmodule.fl_sigs
