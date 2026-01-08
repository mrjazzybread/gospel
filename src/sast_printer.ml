open Sast
open Utils.Fmt

let psymbol fmt p = Ident.pp fmt p.ps_name

let rec sep_terms fmt = list ~sep:star sep_term fmt

and sep_term fmt = function
  | Logical t -> Tast_printer.term fmt t
  | Lift (p, arg1, arg2) ->
      pp fmt "@[%a %a %a@]" psymbol p Tast_printer.print_arg arg1
        Tast_printer.print_arg arg2
  | Wand (t1, t2) -> pp fmt "@[%a -* %a@]" sep_terms t1 sep_terms t2
  | Quant (q, l, t) ->
      pp fmt "@[%a %a, %a@]" Uast_printer.print_quantifier q
        (list ~sep:sp Tast_printer.cast)
        l sep_terms t

let forall fmt () = pp fmt "∀ "
let lam fmt () = pp fmt "λ "
let ex fmt () = pp fmt "∃ "
let period fmt () = pp fmt ".@ "
let quant sym pp = list ~first:sym ~sep:sp ~last:period pp
let forall_quant pp = quant forall pp
let ex_quant pp = quant ex pp
let ret_quant pp = quant lam pp
let tvars_quant = forall_quant Tast_printer.print_tv

let arg_quant ~is_ret fmt l =
  let arg fmt = function
    | Unit | Wildcard -> assert false
    | Ghost v -> pp fmt "%a" (Tast_printer.ts ~top_level:false) v
    | Value v ->
        Ident.pp fmt v.arg_ocaml.ts_id;
        if not is_ret then pp fmt " %a" Ident.pp v.arg_model.ts_id
  in
  let vals = List.filter (function Unit | Wildcard -> false | _ -> true) l in
  if is_ret then ret_quant arg fmt vals else forall_quant arg fmt vals

let triple_app fmt t =
  let nm, args = (t.triple_name, t.triple_args) in
  pp fmt "%a%a" Ident.pp nm
    (list ~first:sp ~sep:sp ~last:sp Ident.pp)
    (List.filter_map
       (function Value v -> Some v.arg_ocaml.ts_id | _ -> None)
       args)

let triple_post fmt (ex, post) =
  pp fmt "%a%a" (ex_quant (Tast_printer.ts ~top_level:false)) ex sep_terms post

let triple fmt t =
  pp fmt "@[Triple :@ %a%a@[<hov2>{ %a }@]@\n@[%a@]@\n@[<hov2>{ %a%a }@]@]"
    tvars_quant
    (t.triple_otvars @ t.triple_gtvars)
    (arg_quant ~is_ret:false) t.triple_args sep_terms t.triple_pre triple_app t
    (arg_quant ~is_ret:true) t.triple_rets triple_post t.triple_post

let rep_pred fmt p =
  pp fmt "@[Predicate %a%a :%a@]" Ident.pp p.Tast.lid
    (list ~first:sp ~sep:sp Tast_printer.print_tv)
    (p.lovars @ p.lgvars)
    (list ~first:sp ~sep:arrow Tast_printer.print_ty)
    [ p.locaml; p.lmodel ]

let tdef fmt = function
  | Abstract -> ()
  | Alias pty -> Tast_printer.print_ty fmt pty
  | Record l ->
      let field fmt (id, t) =
        pp fmt "%a : %a" Ident.pp id Tast_printer.print_ty t
      in
      pp fmt "@[{@ %a@ }@]" (list ~sep:semi field) l

let tdef fmt def = match def with Abstract -> () | _ -> tdef fmt def

let type_decl fmt t =
  pp fmt "Type %a %a %a" Ident.pp t.type_name
    (list ~sep:sp ~last:sp Tast_printer.print_tv)
    t.type_args tdef t.type_def

let axiom fmt ax =
  pp fmt "Axiom %a : %a%a" Ident.pp ax.sax_name tvars_quant ax.sax_tvars
    sep_terms ax.sax_term

let rec definition_node fmt = function
  | Pred p -> rep_pred fmt p
  | Type t -> type_decl fmt t
  | Triple t -> triple fmt t
  | Val v -> Tast_printer.s_val_description fmt v
  | Axiom ax -> axiom fmt ax
  | Function f -> Tast_printer.function_ fmt f
  | Module (id, defs) ->
      pp fmt "Module %a : @\n@[<hov2>%a@]@\n" Ident.pp id definitions defs
  | Import l -> pp fmt "Import %a" Tast_printer.qualid l

and definition fmt x = definition_node fmt x.d_node

and definitions fmt =
  Format.pp_open_hovbox fmt 2;
  newline fmt ();
  list ~sep:(newline ++ newline) definition fmt
