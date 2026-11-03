(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

module W = Warnings
open Ppxlib
open Parse_uast
open Uast_utils

let is_spec attr = attr.attr_name.txt = "gospel"

let rec get_spec_attr = function
  | [] -> (None, [])
  | h :: t when is_spec h -> (Some h, t)
  | h :: t ->
      let elt, rest = get_spec_attr t in
      (elt, h :: rest)

let get_spec_content attr =
  match attr.attr_payload with
  | PStr
      [
        {
          pstr_desc =
            Pstr_eval
              ( {
                  pexp_desc = Pexp_constant (Pconst_string (spec, spec_loc, _));
                  _;
                },
                _ );
          _;
        };
      ] ->
      (spec, spec_loc)
  | _ -> assert false

let get_spec_loc attr = snd (get_spec_content attr)

let parse_gospel ~filename parse attr =
  let spec, spec_loc = get_spec_content attr in
  let lb = Lexing.from_string spec in
  Lexing.set_position lb spec_loc.loc_start;
  Lexing.set_filename lb filename;
  try (spec, parse Ulexer.token lb)
  with Uparser.Error ->
    let loc =
      { loc_start = lb.lex_start_p; loc_end = lb.lex_curr_p; loc_ghost = false }
    in
    W.error ~loc W.Syntax_error

let params_to_id =
  let param_to_id (core_type, _) =
    let loc = core_type.ptyp_loc in
    match core_type.ptyp_desc with
    | Ptyp_var s -> Some (Preid.create s ~loc)
    | Ptyp_any -> None
    | _ -> assert false
    (* There are no other possible values for type parameters in type declarations *)
  in
  map_option param_to_id

let preid_of_loc s = Preid.create ~loc:s.loc s.txt

let rec preid_of_long (loc : location) (s : longident) =
  let preid_of_long = preid_of_long loc in
  match s with
  | Lident id -> Qid (Preid.create ~loc id)
  | Ldot (id, s) -> Qdot (preid_of_long id, Preid.create ~loc s)
  | _ -> assert false

let rec core_to_pty cty =
  let loc = cty.ptyp_loc in
  match cty.ptyp_desc with
  | Ptyp_var str -> PTtyvar (Preid.create ~loc str)
  | Ptyp_constr (id, l) ->
      PTtyapp (preid_of_long id.loc id.txt, List.map core_to_pty l)
  | Ptyp_arrow (_, t1, t2) -> PTarrow (core_to_pty t1, core_to_pty t2)
  | Ptyp_tuple l -> PTtuple (List.map core_to_pty l)
  | _ -> assert false (* TODO replace with unsupported*)

let ptype_kind = function
  | Ptype_abstract | Ptype_variant _ -> PTtype_abstract
  | Ptype_record l ->
      let to_gospel_label l =
        {
          pld_name = preid_of_loc l.Ppxlib.pld_name;
          pld_mutable =
            (match l.pld_mutable with
            | Mutable -> Mutable
            | Immutable -> Immutable);
          pld_type = core_to_pty l.pld_type;
          pld_loc = l.pld_loc;
        }
      in
      PTtype_record (List.map to_gospel_label l)
  | _ -> assert false

let mk_tdecl t attrs spec =
  let* tparams = params_to_id t.ptype_params in
  {
    tname = preid_of_loc t.ptype_name;
    tparams;
    tkind = ptype_kind t.ptype_kind;
    tmanifest = Option.map core_to_pty t.ptype_manifest;
    tattributes = attrs;
    tspec = spec;
    tloc = t.ptype_loc;
  }

let type_declaration ~filename t =
  let spec_attr, other_attrs = get_spec_attr t.ptype_attributes in
  let parse attr =
    let ty_text, spec = parse_gospel ~filename Uparser.type_spec attr in
    let ty_loc = get_spec_loc attr in
    { spec with ty_text; ty_loc }
  in
  let spec = Option.map parse spec_attr in
  mk_tdecl t other_attrs spec

let val_description ~filename v =
  let spec_attr, other_attrs = get_spec_attr v.pval_attributes in
  let parse attr =
    let sp_text, spec = parse_gospel ~filename Uparser.val_spec attr in
    let sp_loc = get_spec_loc attr in
    { spec with sp_text; sp_loc }
  in
  let spec = Option.map parse spec_attr in
  {
    vname = preid_of_loc v.pval_name;
    vtype = core_to_pty v.pval_type;
    vattributes = other_attrs;
    vspec = spec;
    vloc = v.pval_loc;
  }

let floating_spec ~filename a = parse_gospel ~filename Uparser.top a

let sig_exception exn =
  let c = exn.ptyexn_constructor in
  let exn_id = preid_of_loc c.pext_name in
  let exn_loc = c.pext_loc in
  let exn_attributes = c.pext_attributes in
  let exn_args =
    match c.pext_kind with
    | Pext_decl ([], Pcstr_tuple args, _) -> List.map core_to_pty args
    | Pext_rebind _ -> assert false (* Cannot occur on an interface file. *)
    | _ -> assert false
  in
  { exn_id; exn_loc; exn_attributes; exn_args }

(** [signature_item_desc ~filename s] turns the OCaml signature [s] into an
    appropriate Gospel signature. If the signature [s] is unsupported by Gospel
    or is an OCaml value declaration without a specification, we return [None].
*)
let rec signature_item_desc ~filename = function
  | Psig_value v ->
      let v = val_description ~filename v in
      Some (Sig_val v)
  | Psig_type (_, tl) ->
      let* tl = map_option (type_declaration ~filename) tl in
      Sig_type tl
  | Psig_attribute a ->
      if not (is_spec a) then Some (Sig_attribute a)
      else
        let txt, s = floating_spec ~filename a in
        Some (Sig_gospel (s, txt))
  | Psig_module m ->
      let* decl = module_declaration ~filename m in
      Sig_module decl
  | Psig_exception e -> Some (Sig_exception (sig_exception e))
  (* Unsupported *)
  | Psig_recmodule _ -> None
  | Psig_modtype _ -> None
  | Psig_typext _ -> None
  | Psig_open _ -> None
  | Psig_include _ -> None
  | Psig_class _ -> None
  | Psig_class_type _ -> None
  | Psig_extension _ -> None
  | Psig_typesubst _ -> None
  | Psig_modsubst _ -> None
  | Psig_modtypesubst _ -> None

and signature ~filename sigs =
  List.filter_map
    (fun { psig_desc; psig_loc } ->
      let filename =
        match psig_loc.loc_start.pos_fname with
        | "" | "_none_" -> filename
        | f -> f
      in
      let sdesc = signature_item_desc ~filename psig_desc in
      Option.map (fun sdesc -> { sdesc; sloc = psig_loc }) sdesc)
    sigs

and module_type_desc ~filename = function
  | Pmty_signature s -> Some (Mod_signature (signature ~filename s))
  | Pmty_ident _ -> None
  | Pmty_functor _ -> None
  | Pmty_with _ -> None
  | Pmty_typeof _ -> None
  | Pmty_extension _ -> None
  | Pmty_alias _ -> None

and module_type ~filename m =
  let* mdesc = module_type_desc ~filename m.pmty_desc in
  { mdesc; mloc = m.pmty_loc; mattributes = m.pmty_attributes }

and module_declaration ~filename m =
  let loc = m.pmd_name.loc in
  let* nm = m.pmd_name.txt and* mdtype = module_type ~filename m.pmd_type in
  {
    mdname = Preid.create ~loc nm;
    mdtype;
    mdattributes = m.pmd_attributes;
    mdloc = m.pmd_loc;
  }

let unit_pty = PTtyapp (Qid (Preid.create ~loc:Location.none "unit"), [])

let rec fun_args = function
  | Pexp_fun (Nolabel, None, pat, exp) -> (
      let args, typs, ret = fun_args exp.pexp_desc in
      match pat.ppat_desc with
      | Ppat_constraint ({ ppat_desc = Ppat_var v; _ }, ctyp) ->
          let id = preid_of_loc v in
          (Lvar id :: args, core_to_pty ctyp :: typs, ret)
      | Ppat_construct ({ txt = Lident "()"; loc }, None) ->
          (Lunit loc :: args, unit_pty :: typs, ret)
      | _ -> assert false)
  | Pexp_constraint (_, ctyp) -> ([], [], core_to_pty ctyp)
  | _ -> ([], [], unit_pty)

let let_typ exp =
  match exp.pexp_desc with
  | Pexp_fun _ as e ->
      let args, typs, ret = fun_args e in
      let typ = List.fold_right (fun arg ret -> PTarrow (arg, ret)) typs ret in
      (args, typ)
  | _ -> assert false

let value_binding ~filename v =
  let* nm =
    match v.pvb_pat.ppat_desc with
    | Ppat_var v -> Some (preid_of_loc v)
    | _ -> None
  in
  let spec_attr, other_attrs = get_spec_attr v.pvb_attributes in
  let parse attr =
    let sp_text, spec = parse_gospel ~filename Uparser.let_spec attr in
    let sp_loc = get_spec_loc attr in
    let args, typ = let_typ v.pvb_expr in
    let sp_header =
      Some
        {
          sp_hd_nm = nm;
          sp_hd_args = args;
          sp_hd_ret = [ Lvar (Preid.create ~loc:Location.none "result") ];
        }
    in
    let spec = { spec with sp_text; sp_loc; sp_header } in
    (typ, spec)
  in
  let spec = Option.map parse spec_attr in
  let vb =
    {
      pvb_nm = nm;
      pvb_loc = v.pvb_loc;
      pvb_attributes = other_attrs;
      pvb_spec = spec;
    }
  in
  vb

let rec implementation_item_desc ~filename = function
  | Pstr_value (_, [ v ]) ->
      let* v = value_binding ~filename v in
      Str_let v
  | Pstr_value (_, []) -> assert false
  | Pstr_type (_, tl) ->
      let* tl = map_option (type_declaration ~filename) tl in
      Str_type tl
  | Pstr_attribute a ->
      if not (is_spec a) then Some (Str_attribute a)
      else
        let txt, s = floating_spec ~filename a in
        Some (Str_gospel (s, txt))
  | Pstr_exception e -> Some (Str_exception (sig_exception e))
  (* Unsupported *)
  | Pstr_value (_, _ :: _ :: _) -> None
  | Pstr_module _ -> None
  | Pstr_eval _ -> None
  | Pstr_primitive _ -> None
  | Pstr_recmodule _ -> None
  | Pstr_modtype _ -> None
  | Pstr_typext _ -> None
  | Pstr_open _ -> None
  | Pstr_include _ -> None
  | Pstr_class _ -> None
  | Pstr_class_type _ -> None
  | Pstr_extension _ -> None

and signature ~filename sigs =
  List.filter_map
    (fun { psig_desc; psig_loc } ->
      let filename =
        match psig_loc.loc_start.pos_fname with
        | "" | "_none_" -> filename
        | f -> f
      in
      let sdesc = signature_item_desc ~filename psig_desc in
      Option.map (fun sdesc -> { sdesc; sloc = psig_loc }) sdesc)
    sigs

and module_type_desc ~filename = function
  | Pmty_signature s -> Some (Mod_signature (signature ~filename s))
  | Pmty_ident _ -> None
  | Pmty_functor _ -> None
  | Pmty_with _ -> None
  | Pmty_typeof _ -> None
  | Pmty_extension _ -> None
  | Pmty_alias _ -> None

and module_type ~filename m =
  let* mdesc = module_type_desc ~filename m.pmty_desc in
  { mdesc; mloc = m.pmty_loc; mattributes = m.pmty_attributes }

and module_declaration ~filename m =
  let loc = m.pmd_name.loc in
  let* nm = m.pmd_name.txt and* mdtype = module_type ~filename m.pmd_type in
  {
    mdname = Preid.create ~loc nm;
    mdtype;
    mdattributes = m.pmd_attributes;
    mdloc = m.pmd_loc;
  }

let implementation ~filename sigs =
  List.filter_map
    (fun { pstr_desc; pstr_loc } ->
      let filename =
        match pstr_loc.loc_start.pos_fname with
        | "" | "_none_" -> filename
        | f -> f
      in
      let sdesc = implementation_item_desc ~filename pstr_desc in
      Option.map (fun str_desc -> { str_desc; str_loc = pstr_loc }) sdesc)
    sigs
