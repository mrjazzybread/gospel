open Sep_ast
open Utils.Fmt
open Identifier

let field fmt t = 
  pp fmt "@[(%a : %a)@] " 
    Ident.pp t.Symbols.vs_name
    Ttypes.print_ty t.Symbols.vs_ty  

let rec print_term fmt term = 
  match term with 
  |Star l -> list ~sep:star print_term fmt l   
  |App(id, l) -> 
    pp fmt "@[%a@]" Ident.pp id.ls_name;  
    list ~first:lparens ~last:rparens ~sep:comma 
    Ident.pp fmt (List.map (fun x-> x.Symbols.vs_name) l)  
  |RO t ->
    pp fmt "@[RO (%a)@]" print_term t 
  |Pure t ->
    pp fmt "@[[%a]@]" (Tterm_printer.print_term ~print_type:false) t
  |Exists(l, t) ->
    pp fmt "@[∃ %a.@\n@[%a@]]"
    (list 
      (fun fmt x -> 
        pp fmt "(%a : %a)" 
          Ident.pp x.Symbols.vs_name 
          Ttypes.print_ty x.Symbols.vs_ty) 
        ~sep:comma) l 
    print_term t
  |Lambda(ret, s) ->
    pp fmt "@[λ %a.@\n %a@]"
      (list
         (fun fmt x -> 
        pp fmt "(%a : %a)" 
          Ident.pp x.Symbols.vs_name 
          Ttypes.print_ty x.Symbols.vs_ty) 
         ~sep:comma) ret
      print_term s
  |_ -> assert false

let print_app fmt args =
  match args with
  |None -> pp fmt "%s" "()"
  |Some vs -> pp fmt "%s" vs.Symbols.vs_name.id_str


let print_triple fmt t = 
  pp fmt "@[∀ %a. @\n@[{ %a }@]@\n@[%s %a @]@\n{ %a }@]" 
  (list 
        field
        ~sep:comma)
      t.triple_vars
      print_term t.triple_pre
      t.triple_name.id_str
      (list print_app ~sep:sp) t.triple_args
      print_term t.triple_post

let sep_node fmt s = match s.d_node with 
  |Type(t, _, vl) -> pp fmt "@[Type %a %a@]"
                    (list Ttypes.print_tv) vl
                    Ident.pp t
|Pred(id, args) -> 
    pp fmt "@[Predicate %a %a@]" 
      Ident.pp id 
      (fun fmt args -> List.iter (field fmt) args) args
|Triple t -> 
  pp fmt "@[Triple %a :@\n %a@]" 
    Ident.pp t.triple_name
    print_triple t 
|Axiom axiom -> 
  Tast.pp_axiom fmt axiom
|Function f -> Tast.pp_function_ fmt f
let file fmt l = list ~sep:(newline ++ newline) sep_node fmt l
