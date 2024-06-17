open Sep_ast
open Utils.Fmt
open Identifier

let field fmt t =
  pp fmt "@[(%a : %a)@] " Ident.pp t.Symbols.vs_name Ttypes.print_ty
    t.Symbols.vs_ty

let print_term fmt term =
  match term with
  | App (id, l) ->
      pp fmt "@[%a@]" Ident.pp id.ls_name;
      list ~first:lparens ~last:rparens ~sep:comma
        (Tterm_printer.print_term ~print_type:false)
        fmt l
  | Pure t -> pp fmt "@[[%a]@]" (Tterm_printer.print_term ~print_type:false) t

let print_app fmt args =
  match args with
  | None -> pp fmt "%s" "()"
  | Some vs -> pp fmt "%s" vs.Symbols.vs_name.id_str

let print_rets fmt r =
  if r = [] then () else pp fmt "@[λ %a.@]" (list field ~sep:sp) r

let print_post fmt (vl, tl) =
  pp fmt "@[∃ %a.@\n@[%a@]]"
    (list
       (fun fmt x ->
         pp fmt "(%a : %a)" Ident.pp x.Symbols.vs_name Ttypes.print_ty
           x.Symbols.vs_ty)
       ~sep:comma)
    vl
    (list print_term ~sep:star)
    tl

let print_triple fmt t =
  pp fmt "@[∀ %a. @\n@[{ %a }@]@\n@[%s %a @]@\n{%a %a }@]"
    (list field ~sep:comma) t.triple_vars
    (list print_term ~sep:star)
    t.triple_pre t.triple_name.id_str (list print_app ~sep:sp) t.triple_args
    print_rets t.triple_rets print_post t.triple_post

let rec sep_node fmt s =
  match s.d_node with
  | Type tdef ->
      pp fmt "@[Type %a %a@]" (list Ttypes.print_tv) tdef.type_args Ident.pp
        tdef.type_name
  | Pred pred ->
      pp fmt "@[Predicate %a %a@]" Ident.pp pred.pred_name
        (fun fmt args -> List.iter (field fmt) args)
        pred.pred_args
  | Triple t ->
      pp fmt "@[Triple %a :@\n%a@]" Ident.pp t.triple_name print_triple t
  | Axiom (_, axiom) -> Tast_printer.print_axiom fmt axiom
  | Function (_, f) -> Tast_printer.print_function fmt f
  | Module (nm, l) ->
      pp fmt "@[Module %a :@\n%a@]" Ident.pp nm (list sep_node ~sep:newline) l

let file fmt l = list ~sep:(newline ++ newline) sep_node fmt l
