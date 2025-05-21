open Id_uast
open Tast
open Sep_utils
open Sast

let cons x l = match x with None -> l | Some x -> x :: l

(** Translates a Gospel type declaration into 1-3 Separation Logic definitions.
*)
let type_declaration ~ocaml ns t =
  (* Creates a type declaration for the model. If the model has no
     named model fields, then this function returns None *)
  let model_decl model_type =
    match model_type with
    | Id_uast.Fields fields ->
        let fields = List.map (fun x -> (x.pld_name, x.pld_type)) fields in
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

  let spec = t.tspec in
  let model_type = spec.ty_model in
  let is_record = match model_type with Fields _ -> true | _ -> false in
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
  let pred_def = List.map (fun x -> Pred x) spec.ty_lenses in
  let () = List.iter (Sep_utils.map_pred ns) spec.ty_lenses in
  cons type_decl (cons model_decl pred_def)

(** Transforms a single Gospel top level declaration into potentially several
    Separation Logic definitions *)
let rec signature_item_desc ns = function
  | Tast.Sig_type l -> List.concat_map (type_declaration ns ~ocaml:true) l
  | Sig_ghost_type l -> List.concat_map (type_declaration ns ~ocaml:false) l
  | Sig_function f -> [ Function f ]
  | _ -> []

and signature_item ns s =
  let sigs = signature_item_desc ns s.sdesc in
  let sigs = List.map (fun sep -> { d_node = sep; d_loc = s.sloc }) sigs in
  sigs

let process_sigs file =
  let ns = empty_env () in
  let f s =
    let sigs = signature_item ns s in
    sigs
  in
  List.concat_map f file
