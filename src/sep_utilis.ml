open Symbols
open Ttypes
open Ident
module Mstr = Tmodule.Mstr

type sep_namespace = {
    sns_pred : lsymbol Mstr.t;
    sns_id : vsymbol Mstr.t;
    sns_ns : sep_namespace Mstr.t;
  }

let empty_ns = {
    sns_pred = Mstr.empty;
    sns_id   = Mstr.empty;
    sns_ns   = Mstr.empty;
  }


let ts_loc = mk_ts
               (Ident.create ~loc:Location.none "loc")
               [fresh_tv ~loc:Location.none "a" ]
let ts_prop = mk_ts
                (Ident.create ~loc:Location.none "Prop")
                []

let ty_prop = ty_app ts_prop []
let ty_loc t = ty_app ts_loc [t]

let rep_pred_pref = "@R_"
let get_rep_pred s = rep_pred_pref ^ s

let val_pref = "@V_"
let mk_val s = val_pref ^ s
let mk_update s =  s ^ "'"

let change_id id map =
  Ident.create
    ~attrs:id.id_attrs
    ~loc:id.id_loc
    (map id.id_str)

let map_id ns vs ty =
  let id = change_id vs.vs_name mk_val in
  let id =
    if Mstr.mem id.id_str ns.sns_id then
      change_id id mk_update
    else
      id in  
  let val_vs = {vs_name = id; vs_ty = ty} in
  let new_map = Mstr.add id.id_str val_vs ns.sns_id in
  val_vs, {ns with sns_id = new_map}

let get_id ns is_old id =
  let map = if is_old then mk_val else mk_update in
  let name = map id in 
  try Mstr.find name ns.sns_id with
  |Not_found -> Mstr.find (mk_val id) ns.sns_id
    
let is_present ns id =
  let str = id.id_str in 
  Mstr.mem (mk_val str) ns.sns_id

let create_rep_pred sym =
  let id = sym.ts_ident in
  let self_type = {ty_node=Tyapp (sym, List.map (fun x -> {ty_node=Tyvar x}) sym.ts_args)} in 
  let model_type = match sym.ts_rep with
    |Self -> self_type
    |Model m -> m in 
  let new_id = change_id id get_rep_pred in
  {
    ls_name = new_id;
    ls_args = [self_type; model_type];
    ls_value = None;
    ls_constr = false;
    ls_field = false;
  } 
  

let get_pred ns ty =
  match ty.ty_node with
  |Tyapp (ts, _) ->
    begin try Mstr.find ts.ts_ident.id_str ns.sns_pred with
    |Not_found -> create_rep_pred ts end
  |_ -> assert false

let id_of_term t = match t.Tterm.t_node with
  |Tterm.Tvar v -> v
  |_ -> assert false

let get_ty_name ty = match ty.ty_node with
  |Tyapp (t, _) -> t.ts_ident.id_str
  |Tyvar v -> v.tv_name.id_str

