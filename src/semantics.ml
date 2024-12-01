(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

open Ttypes
open Tast
open Symbols
open Tterm
open Sep_utilis
open Sast

type spatial_info = {
  arg_pred : psymbol;  (** Representation Predicate *)
  arg_prog : vsymbol;  (** OCaml value *)
  ro : bool;  (** Read only flag *)
}
(** Contains the ownership information for a variable within a specification *)

type value = {
  arg_log : vsymbol;  (** Logical value *)
  arg_spatial : spatial_info option;
      (** Ownership information. None if the ownership is duplicable *)
}

let to_prog arg_vs =
  let arg_id = change_id prog_var arg_vs.vs_name in
  let arg_prog_ty =
    if is_pure_type arg_vs then to_prog_type arg_vs.vs_ty
    else Ttypes.ty_loc arg_vs.vs_ty
  in
  { vs_name = arg_id; vs_ty = arg_prog_ty }

let mk_lift { arg_spatial; arg_log } =
  Option.map
    (fun spatial ->
      Lift
        ( spatial.arg_pred,
          [
            Tterm_helper.t_var spatial.arg_prog Location.none;
            Tterm_helper.t_var arg_log Location.none;
          ] ))
    arg_spatial

(** Transforms a Gospel term into a Separation Logic term. In most cases, the
    original term will be unchaged. However, when we have a quantification over
    an ephemeral OCaml type, we magic wand term which combined with ownership of
    the ephemeral value will produce the axiom *)
let tterm_to_sep ns t =
  (* This flag will track if the axiom we are building has any magic
     wands. If not, we return the original Gospel term. *)
  let is_pure = ref true in
  let rec loop ns t =
    (* Creates ownership information for a quantified variable*)
    let quant_lift binder =
      let pred = get_pred ns binder.bind_prog in
      let spatial_info =
        Option.map
          (fun pred ->
            { arg_pred = pred; arg_prog = to_prog binder.bind_vs; ro = true })
          pred
      in
      { arg_log = binder.bind_vs; arg_spatial = spatial_info }
    in

    match t.t_node with
    | Tquant (q, l, t) -> (
        let vlist = List.map quant_lift l in
        let lifts = List.filter_map mk_lift vlist in
        let vsl =
          List.concat_map
            (fun x ->
              Option.fold ~none:[] ~some:(fun x -> [ x.arg_prog ]) x.arg_spatial
              @ [ x.arg_log ])
            vlist
        in
        let term = loop ns t in
        match lifts with
        | [] ->
            [ Quant (q, vsl, term) ]
            (* All variables are of non-ephemeral types *)
        | _ ->
            let () = is_pure := false in
            [ Quant (q, vsl, [ Wand (lifts, term @ lifts) ]) ])
    | Tbinop (Tand, t1, t2) -> loop ns t1 @ loop ns t2
    | _ -> [ Pure t ]
  in
  let sept = loop ns t in
  if !is_pure then [ Pure t ] else sept

(** [map_term env ns is_old t] maps a Gospel specification term into a
    Separation Logic term. This function first replaces any modified variable
    outside of an [old] block with the corresponding variable within [env]. It
    then transforms the Gospel term as is specified in [tterm_to_sep] *)
let map_term env ns is_old t =
  let rec change_vars is_old t =
    let f t = change_vars is_old t in
    let map_node =
      match t.t_node with
      | Tvar v when (not is_old) && is_present env v.vs_name ->
          Tvar (get_id env v.vs_name.id_str)
      | Tlet (v, t1, t2) -> Tlet (v, f t1, f t2)
      | Told t -> (change_vars true t).t_node
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
  in
  change_vars is_old t |> tterm_to_sep ns

(** Gets the polymorphic variables of the logical type of a Separation Logic
    variable *)
let get_value_poly l = ty_poly (List.map (fun x -> x.arg_log.vs_ty) l)

(** Translates a value description into a Separation Logic triple. *)
let val_description ns des =
  let env = ref empty_env in
  (* Turns a typed argument into a record consisting of the variable's
     ownership conditions and its logical type *)
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
              let arg_log, env' = map_id !env (is_old || ro) arg_vs l_ty in
              let () = env := env' in
              let pred = get_pred ns s_ty in
              let arg_prog = to_prog arg.lb_vs in
              let arg_spatial =
                Option.map (fun arg_pred -> { arg_pred; arg_prog; ro }) pred
              in
              Some { arg_spatial; arg_log })
  in
  let spec = des.vd_spec in

  let lifts = List.filter_map mk_lift in
  let args = lifted_args true spec.sp_args in
  (* Gets the program value for the argument. If there is no
     ownership condition, the program value is equivalent to the
     logical value*)
  let to_prog_arg arg =
    match arg.arg_spatial with Some s -> s.arg_prog | None -> arg.arg_log
  in

  let prog_args = List.map to_prog_arg args in

  (* Gospel preconditions tranformed into Separation Logic terms *)
  let pre = List.concat_map (fun t -> map_term !env ns true t) spec.sp_pre in

  (* Gospel preconditions joined with Separation Logic ownership
     conditions for the function's arguments *)
  let triple_pre = lifts args @ pre in
  let updates = lifted_args false spec.sp_args in
  let rets = lifted_args false spec.sp_ret in

  (* Gospel postconditions tranformed into Separation Logic terms *)
  let post = List.concat_map (fun t -> map_term !env ns false t) spec.sp_post in
  let triple_poly = get_value_poly (args @ rets) in

  (* Gospel postconditions joined with Separation Logic ownership
     conditions for the function's arguments and return values*)
  let post_cond = lifts (updates @ rets) @ post in

  (* Determines what variables require and existentially
     quantified model in the postcondition *)
  let mk_updates = function
    | { arg_log; arg_spatial } ->
        Option.bind arg_spatial (fun x -> if x.ro then None else Some arg_log)
  in

  let updated_vars = List.filter_map mk_updates (updates @ rets) in
  let triple_post = (updated_vars, post_cond) in
  Triple
    {
      triple_name = des.vd_name;
      triple_vars =
        List.concat_map
          (function
            | { arg_spatial; arg_log } -> (
                let l =
                  if arg_log.vs_name.id_str = "()" then [] else [ arg_log ]
                in
                match arg_spatial with Some s -> s.arg_prog :: l | None -> l))
          args;
      triple_args = prog_args;
      triple_rets = List.map to_prog_arg rets;
      triple_checks = spec.sp_checks;
      triple_pre;
      triple_poly;
      triple_type = des.vd_type;
      triple_post;
    }

(** Translates a Gospel type declaration into 1-3 Separation Logic definitions.
    These are as follows:

    - If the model has at least one named field, generates a type definition for
      a record that will serve as the model for the type where each field has
      the same name and type as the Gospel model. In this case, the record type
      will have the same name as the original OCaml type.

    - If the type is not labelled as ephemeral, generates a type declaration for
      the OCaml type. If the type has any named model fields, then the name of
      this type will be the name of the OCaml type with an '_' at the start.

    - If the type has a model that is not isomorphic to its OCaml type,
      generates an abstract representation predicate that lifts the OCaml value
      into a logical representation *)
let type_declaration t =
  (* Creates a type declaration for the model. If the model has no
     named model fields, then this function returns None *)
  let model_decl model_type ts =
    match model_type with
    | Tast.Fields l ->
        let fields =
          List.map
            (fun (_, ls) ->
              let ls_name, _, ls_value = match_field ls in
              let id = ls_name in
              let ty = ls_value in
              (id, ty))
            l
        in
        let def = Record fields in
        Some
          (Type
             { type_name = ts.ts_ident; type_args = ts.ts_args; type_def = def })
    | _ -> None
  in

  (* Generates a representation predicate in which the first argument
     is the OCaml value and the second is its logical model. If the
     logical model is isomorphic to the OCaml type, then it returns
     None *)
  let pred_def ts model_type is_mutable type_name tvar_list =
    let prog_ts = { ts with ts_ident = type_name } in
    let prog_ty = { ty_node = Tyapp (prog_ts, tvar_list) } in
    let pred_prog_ty = if is_mutable then Ttypes.ty_loc prog_ty else prog_ty in

    let prog_vs =
      {
        vs_name = Ident.create "target" ~loc:Location.none;
        vs_ty = pred_prog_ty;
      }
    in

    let pred_model_ty =
      match model_type with
      | Self -> pred_prog_ty
      | Fields _ -> { ty_node = Tyapp (ts, tvar_list) }
      | Default (_, t) -> t
    in
    let model_vs =
      {
        vs_name = Ident.create "model" ~loc:Location.none;
        vs_ty = pred_model_ty;
      }
    in
    let new_id = ts.ts_ident |> change_id rep_pred in
    let pred_type = if is_mutable then Heap else Persistent in
    if model_type = Self && not is_mutable then None
    else
      Some
        (Pred
           {
             pred_name = new_id;
             pred_args = [ prog_vs; model_vs ];
             pred_poly = ts.ts_args;
             pred_type;
           })
  in

  let ts = t.td_ts in
  let spec = t.td_spec in
  let tvar_list = List.map (fun (x, _) -> { ty_node = Tyvar x }) t.td_params in
  let model_type = spec.ty_model in
  let is_record = match model_type with Tast.Fields _ -> true | _ -> false in
  let is_mutable = spec.ty_ephemeral in
  let type_name =
    if is_record then change_id (( ^ ) "_") ts.ts_ident else ts.ts_ident
  in

  let type_decl =
    if not is_mutable then
      Some (Type { type_name; type_args = ts.ts_args; type_def = Abstract })
    else None
  in
  let model_decl = model_decl model_type ts in
  let pred_def = pred_def ts model_type is_mutable type_name tvar_list in

  let cons x l = match x with None -> l | Some x -> x :: l in
  cons type_decl (cons model_decl (cons pred_def []))

(** Transforms a single Gospel top level declaration into potentially several
    Separation Logic definitions *)
let signature_item_desc ns = function
  | Sig_type (_, l, _) -> List.concat_map (fun t -> type_declaration t) l
  | Sig_val (des, _) -> [ val_description ns des ]
  | Sig_function f -> [ Function (function_poly f, f) ]
  | _ -> []

(** [update_ns ns s] when [s] is the declaration of a representation predicate
    returns an updated namespace that maps the name of the predicate to the
    corresponding [lsymbol]. In all other cases, returns [ns] unchanged *)
let update_ns ns s =
  match s with
  | Pred r ->
      let args = List.map (fun x -> x.vs_ty) r.pred_args in
      map_pred ns r.pred_name args
  | _ -> ns

let signature_item ns s =
  let sigs = signature_item_desc ns s.sig_desc in
  let ns = List.fold_left update_ns ns sigs in
  let sigs = List.map (fun sep -> { d_node = sep; d_loc = s.sig_loc }) sigs in
  (ns, sigs)

let process_sigs file =
  let ns = ref empty_module in
  let f s =
    let ns', sigs = signature_item !ns s in
    let () = ns := ns' in
    sigs
  in
  List.concat_map f file.Tmodule.fl_sigs
