open Sep_ast
open Utils.Fmt
open Identifier

let field fmt t = 
  pp fmt "@[(%a : %a)@] " 
    Ident.pp t.Symbols.vs_name
    Ttypes.print_ty t.Symbols.vs_ty  

let rec print_term fmt term = 
  match term.s_node with 
  |Star l -> list ~sep:star print_term fmt l   
  |App(id, l) -> 
    pp fmt "@[%s @]" id;  
    list (fun fmt s -> pp fmt "%s " s) fmt l  
  |RO t ->
    pp fmt "@[RO (%a)@]" print_term t 
  |Pure t ->
    pp fmt "[@[%a]@]" Tterm_printer.print_term t
  |_ -> assert false

let print_triple fmt t = 
  pp fmt "@[{ %a }@\n%a@\n{ %a }@]" 
    print_term t.triple_pre
    (list 
      (fun fmt x -> pp fmt "(%a : %a)" Ident.pp x.Symbols.vs_name Ttypes.print_ty x.Symbols.vs_ty) ~sep:arrow)
      t.triple_args
    print_term t.triple_post

let sep_node fmt s = match s.d_node with 
|Type t -> pp fmt "@[Type %a@]" Ident.pp t
|Pred(id, args) -> 
    pp fmt "@[Predicate %s %a@]" 
      id.id_str 
      (fun fmt args -> List.iter (field fmt) args) args
|Triple t -> 
  pp fmt "@[Definition %a :@\n %a@]" 
    Ident.pp t.triple_name
    print_triple t 



let file fmt l = list ~sep:(newline ++ newline) sep_node fmt l