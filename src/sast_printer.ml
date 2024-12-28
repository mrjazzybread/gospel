open Sast
open Utils.Fmt
open Identifier

let field fmt t =
  pp fmt "(%a : %a)" Ident.pp_simpl t.Symbols.vs_name Ttypes.print_ty
    t.Symbols.vs_ty

let print_gospel_term = Tterm_printer.print_term ~print_type:false

let rec print_term fmt term =
  match term with
  | Lift (id, l) ->
      pp fmt "@[%a@]" Ident.pp_simpl id.ps_name;
      list ~first:lparens ~last:rparens ~sep:comma print_gospel_term fmt l
  | Pure t -> pp fmt "@[[%a]@]" print_gospel_term t
  | Wand (s, l) -> pp fmt "@[%a -*> %a@]" print_terms s print_terms l
  | Quant (q, vl, tl) ->
      let s = match q with Tforall -> "∀" | Texists -> "∃" in
      pp fmt "@[%s %a. @[%a@]]" s (list field ~sep:comma) vl print_terms tl

and print_terms fmt l = list print_term ~sep:star fmt l

let print_app fmt vs = pp fmt "%s" vs.Symbols.vs_name.id_str
let dot fmt _ = pp fmt "."
let print_terms = list ~sep:star print_term

let print_post rets fmt (vl, tl) =
  if tl = [] then ()
  else
    let first_lm fmt _ = pp fmt "λ " in
    let first_ex fmt _ = pp fmt "∃ " in
    let last fmt _ = pp fmt ".@\n" in
    pp fmt "@[%a%a  @[%a@]@]"
      (list ~first:first_lm field ~sep:comma ~last)
      rets
      (list ~first:first_ex field ~sep:comma ~last)
      vl print_terms tl

let print_targs none fmt l =
  match l with
  | [] -> pp fmt "%s" none
  | _ -> pp fmt "@[∀ %a @]" (list ~sep:sp Ttypes.print_tv ~last:dot) l

let print_xpost fmt x =
  pp fmt "@[{ %a %a-> %a}@]" Ident.pp_simpl x.xid.xs_ident
    (list ~sep:sp ~last:sp print_app)
    x.xrets print_terms x.xterm

let print_triple fmt t =
  pp fmt "@[%a%a @\n{@[ %a @]}@\n@[%s %a @]@\n{@[ %a @]}%a@]" (print_targs "∀")
    t.triple_poly
    (list field ~sep:comma ~last:dot)
    t.triple_vars print_terms t.triple_pre t.triple_name.id_str
    (list print_app ~sep:sp) t.triple_args (print_post t.triple_rets)
    t.triple_post
    (list ~first:newline ~sep:newline print_xpost)
    t.triple_xposts

let print_rec_field fmt (id, ty) =
  pp fmt "@[%a : %a@]" Ident.pp_simpl id Ttypes.print_ty ty

let print_type_def fmt t =
  match t with
  | Abstract -> ()
  | Record l -> pp fmt "@[ =@[ { %a }@]@]" (list print_rec_field ~sep:semi) l

let print_pred_type fmt = function Persistent -> pp fmt "Pure " | Heap -> ()
let print_fun_def fmt t = pp fmt "=@\n %a" print_gospel_term t

let func_spec f x =
  pp f "%a%a%a%a"
    (fun f _ -> if x.Tast.fun_coer then pp f "@\ncoercion" else ())
    ()
    (list
       ~first:(newline ++ const string "variant ")
       ~sep:(newline ++ const string "variant ")
       print_gospel_term)
    x.fun_variant
    (list
       ~first:(newline ++ const string "requires ")
       ~sep:(newline ++ const string "requires ")
       print_gospel_term)
    x.fun_req
    (list
       ~first:(newline ++ const string "ensures ")
       ~sep:(newline ++ const string "ensures ")
       print_gospel_term)
    x.fun_ens

let rec sep_node fmt s =
  match s.d_node with
  | Type tdef ->
      pp fmt "@[Type %a%a%a@]" (print_targs "") tdef.type_args Ident.pp_simpl
        tdef.type_name print_type_def tdef.type_def
  | Pred pred ->
      pp fmt "@[%aPredicate %a%a %a@]" print_pred_type pred.pred_type
        (print_targs "") pred.pred_poly Ident.pp_simpl pred.pred_name
        (list field ~sep:sp) pred.pred_args
  | Triple t ->
      pp fmt "@[Triple %a :@\n%a@]" Ident.pp_simpl t.triple_name print_triple t
  | Axiom (_, axiom) ->
      pp fmt "@[Axiom %a :@\n%a@]" Ident.pp_simpl axiom.sax_name print_terms
        axiom.sax_term
  | Function (l, f) ->
      pp fmt "@[Function %a %a%a : %a%a%a@]" Ident.pp_simpl
        (Symbols.get_name f.fun_ls)
        (print_targs "") l (list ~sep:sp field) f.fun_params Ttypes.print_ty
        (Symbols.get_value f.fun_ls)
        (option print_fun_def) f.fun_def (option func_spec) f.fun_spec
  | Module (nm, l) ->
      pp fmt "@[Module %a :@\n@[  %a@]@\nend@]" Ident.pp_simpl nm
        (list sep_node ~sep:newline)
        l
  | Import l -> pp fmt "@[Open %a@]" (list Format.pp_print_string ~sep:full) l

let file fmt l = list ~sep:(newline ++ newline) sep_node fmt l
