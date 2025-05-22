open Id_uast
open Tast
open Sep_utils
open Sast
module Env = Ident.IdTable

type spatial_info = {
  arg_pred : Sast.sep_term;  (** Representation Predicate *)
  arg_log : tsymbol;  (** Logical representation *)
  ro : bool;  (** Read only flag *)
}
(** Contains the ownership information for a variable within a specification *)

type val_info = { arg_val : triple_val; arg_spatial : spatial_info option }

let ty_loc =
  let app = Types.mk_info (Qid (Ident.mk_id "loc")) in
  PTtyapp (app, [])

let cons x l = match x with None -> l | Some x -> x :: l

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
let type_declaration ~ocaml ns t =
  (* Creates a type declaration for the model. If the model has no
     named model fields, then this function returns None *)
  let model_decl model_type =
    match model_type with
    | Id_uast.Fields fields ->
        let def = Record fields in
        Some
          (Type
             {
               type_name = t.tname;
               type_args = t.tparams;
               type_ocaml = false;
               type_def = def;
             })
    | _ -> None
  in

  (* Generates a representation predicate in which the first argument
     is the OCaml value and the second is its logical model. If the
     logical model is isomorphic to the OCaml type, then it returns
     [None] *)
  let pred_def model_type is_mutable tvar_list =
    let app = Types.mk_info (Qid t.tname) in
    let prog_ty = PTtyapp (app, tvar_list) in
    let pred_prog_ty = if is_mutable then ty_loc else prog_ty in

    let prog_vs = mk_ts (Ident.mk_id "target") pred_prog_ty in

    let pred_model_ty =
      match model_type with
      | No_model -> pred_prog_ty
      | Implicit t -> t
      | Fields _ -> PTtyapp (app, tvar_list)
    in
    let model_vs = mk_ts (Ident.mk_id "model") pred_model_ty in
    let new_id = change_id rep_pred t.tname in
    let pred_type = [ pred_prog_ty; pred_model_ty ] in
    let () = Sep_utils.map_pred ns t.tname.id_tag new_id is_mutable pred_type in
    let pred_kind = if is_mutable then Heap else Pure in
    Pred
      {
        pred_name = new_id;
        pred_args = [ prog_vs; model_vs ];
        pred_poly = t.tparams;
        pred_kind;
      }
  in

  let spec = t.tspec in
  let tvar_list = List.map (fun x -> PTtyvar x) t.tparams in
  let model_type = spec.ty_model in
  let is_record = match model_type with Fields _ -> true | _ -> false in
  let is_mutable = spec.ty_mutable in
  let type_name =
    if is_record then change_id (( ^ ) "_") t.tname else t.tname
  in

  let type_decl =
    if not ocaml then
      let tdef =
        match t.tmanifest with Some t -> Alias t | None -> Abstract
      in
      Some
        (Type
           {
             type_name;
             type_args = t.tparams;
             type_ocaml = false;
             type_def = tdef;
           })
    else None
  in
  let model_decl = model_decl model_type in
  let pred_def =
    if ocaml then Some (pred_def model_type is_mutable tvar_list) else None
  in

  cons type_decl (cons model_decl (cons pred_def []))

let is_present env v =
  let id = Uast_utils.leaf v in
  Env.mem env id.id_tag

(** [map_term env ns is_old t] maps a Gospel specification term into a
    Separation Logic term. This function first replaces any modified variable
    outside of an [old] block with the corresponding variable within [env]. It
    then transforms the Gospel term as is specified in [tterm_to_sep] *)
let map_term env is_old t =
  let rec change_vars is_old t =
    let f t = change_vars is_old t in
    let map_node =
      match t.t_node with
      | Tvar v when (not is_old) && is_present env v ->
          Tvar (Env.find env (Uast_utils.leaf v).id_tag)
      | Tlet (v, t1, t2) -> Tlet (v, f t1, f t2)
      | Told t -> (change_vars true t).t_node
      | Tapply (t1, t2) -> Tapply (f t1, f t2)
      | Tif (t1, t2, t3) -> Tif (f t1, f t2, f t3)
      | Tquant (q, l, t) -> Tquant (q, l, f t)
      | Tfield (t, n) -> Tfield (f t, n)
      | Tscope (q, t) -> Tscope (q, f t)
      | _ -> t.t_node
    in
    { t with t_node = map_node }
  in
  Logical (change_vars is_old t)

let update_var s = "_" ^ s ^ "'"
let prog_var s = "__" ^ s

let map_id env is_old nm =
  let needs_updated_var = (not is_old) && is_present env nm in
  let id = Uast_utils.leaf nm in
  let id' = if needs_updated_var then change_id update_var id else id in

  Env.add env id.id_tag (Qid id');
  id'

let to_prog arg =
  let arg_id = change_id prog_var (Uast_utils.leaf arg.var_name) in
  mk_ts arg_id arg.ty_ocaml

let lifted_arg ns env is_pre arg =
  match arg with
  | Id_uast.Unit -> { arg_spatial = None; arg_val = Unit }
  | Wildcard -> { arg_spatial = None; arg_val = Wildcard }
  | Ghost (id, ty) ->
      { arg_spatial = None; arg_val = Ghost { ts_id = id; ts_ty = ty } }
  | OCaml v ->
      let ro = v.ro in
      let arg_log = map_id env (is_pre || ro) v.var_name in
      let arg_ts = mk_ts arg_log (Option.value ~default:ty_loc v.ty_gospel) in
      let pred = get_pred ns v.ty_ocaml in
      let arg_prog = to_prog v in
      let to_term v = mk_term (Tvar (Qid v.ts_id)) v.ts_ty Location.none in
      let arg_spatial =
        Option.map
          (fun pred ->
            let arg_pred = Lift (pred, to_term arg_prog, to_term arg_ts) in
            { arg_pred; arg_log = arg_ts; ro })
          pred
      in
      let arg_val =
        Sast.Value
          {
            arg_ocaml = arg_prog;
            arg_model = arg_ts;
            is_loc = Uast_utils.can_own v.ty_ocaml;
          }
      in
      { arg_spatial; arg_val }

let val_des des spec ns =
  let env = Env.create 100 in
  let lift is_old = List.map (lifted_arg ns env is_old) in
  let args = (lift true) spec.sp_args in
  let update_args = (lift false) spec.sp_args in
  let rets = (lift false) spec.sp_rets in

  let pre = List.map (fun t -> map_term env true t) spec.sp_pre in
  let post = List.map (fun t -> map_term env false t) spec.sp_post in

  let lifts l =
    List.filter_map (fun x -> Option.map (fun x -> x.arg_pred) x.arg_spatial) l
  in
  let triple_pre = lifts args @ pre in
  let triple_post = lifts (update_args @ rets) @ post in
  let mk_updates arg =
    Option.bind arg.arg_spatial (fun x -> if x.ro then None else Some x.arg_log)
  in
  let updated_vars = List.filter_map mk_updates update_args in
  let ret_vars = List.filter_map mk_updates rets in
  let triple_post = (updated_vars @ ret_vars, triple_post) in
  (* Gets the program value for the argument. *)
  let to_prog_arg = fun arg -> arg.arg_val in
  let def =
    {
      triple_name = des.vname;
      triple_args = List.map to_prog_arg args;
      triple_rets = List.map to_prog_arg rets;
      triple_pre;
      triple_poly = des.vtvars;
      triple_post;
    }
  in
  Triple (Sep_utils.inline_def def)

(** Transforms a single Gospel top level declaration into potentially several
    Separation Logic definitions *)
let signature_item_desc ns = function
  | Tast.Sig_type l -> List.concat_map (type_declaration ns ~ocaml:true) l
  | Sig_ghost_type l -> List.concat_map (type_declaration ns ~ocaml:false) l
  | Sig_function f -> [ Function f ]
  | Sig_value v -> (
      match v.vspec with
      | Some spec -> [ Val v; val_des v spec ns ]
      | None -> [ Val v ])
  | Sig_axiom axiom ->
      let axiom =
        {
          sax_name = axiom.ax_name;
          sax_tvars = axiom.ax_tvars;
          sax_loc = axiom.ax_loc;
          sax_term = [ Logical axiom.ax_term ];
        }
      in
      [ Axiom axiom ]
  | _ -> []

let signature_item env s =
  let sigs = signature_item_desc env s.sdesc in
  let sigs = List.map (fun sep -> { d_node = sep; d_loc = s.sloc }) sigs in
  sigs

let process_sigs file =
  let env = empty_env () in
  let f s =
    let sigs = signature_item env s in
    sigs
  in
  List.concat_map f file
