open Id_uast
open Tast
open Sep_utils
open Sast

let ty_loc =
  let app = Types.mk_info (Qid (Ident.mk_id "loc")) in
  PTtyapp (app, [])

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
let type_declaration ~ocaml t =
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
    let pred_kind = if is_mutable then Heap else Pure in
    if model_type = No_model && not is_mutable then None
    else
      Some
        (Pred
           {
             pred_name = new_id;
             pred_args = [ prog_vs; model_vs ];
             pred_poly = t.tparams;
             pred_kind;
           })
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
    if ocaml then pred_def model_type is_mutable tvar_list else None
  in

  let cons x l = match x with None -> l | Some x -> x :: l in
  cons type_decl (cons model_decl (cons pred_def []))

(** Transforms a single Gospel top level declaration into potentially several
    Separation Logic definitions *)
let signature_item_desc = function
  | Tast.Sig_type l -> List.concat_map (type_declaration ~ocaml:true) l
  | Sig_ghost_type l -> List.concat_map (type_declaration ~ocaml:false) l
  | _ -> []

let signature_item s =
  List.map
    (fun sep -> { d_node = sep; d_loc = s.sloc })
    (signature_item_desc s.sdesc)

let process_sigs = List.concat_map (fun s -> signature_item s)
