open Sep_ast
open Utils.Fmt
open Identifier

let field fmt t = 
  pp fmt "@[(%a : %a)@] " 
    Ident.pp t.f_name
    Ttypes.print_ty t.f_type  

let sep_node fmt s = match s.d_node with 
|Type t -> pp fmt "@[Type %a@]" Ident.pp t.t_name
|Pred(id, args) -> 
    pp fmt "@[Predicate %s %a@]" 
      id.id_str 
      (fun fmt args -> List.iter (field fmt) args) args
|_ -> assert false

let file fmt l = list ~sep:(newline ++ newline) sep_node fmt l