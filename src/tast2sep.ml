open Tterm
open Tast
open Sep_ast
open Symbols
module Env = Map.Make(String)


(*type _env = bool Env.t*)

let env = ref (Env.empty)

let rec signature_item_desc s = match s with 
|Sig_type(_, l, _) -> List.concat_map (fun t -> type_declaration t) l 
|_ -> assert false

and type_declaration t = 
let model_types = 
  match t.td_spec with
  |Some s ->  
    let model_to_arg (sym, mut) = 
      let arg = sym.ls_name in
      let ty = match sym.ls_value with |Some t -> t |None -> assert false in 
      let field = {f_name = arg; f_type = ty} in 
      if mut then [field] else [field; field] in
    List.concat_map model_to_arg s.ty_fields
  |None -> [] in 

[Type t; Pred(t.td_ts.ts_ident, model_types)]

let signature_item s = 
  List.map (fun sep -> {d_node = sep; d_loc = s.sig_loc}) (signature_item_desc s.sig_desc)