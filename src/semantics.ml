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
let signature_item_desc = function
  | Sig_type (_, l, _) -> List.concat_map (fun t -> type_declaration t) l
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
  let sigs = signature_item_desc s.sig_desc in
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
