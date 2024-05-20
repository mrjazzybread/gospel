open Symbols
open Ttypes
open Ident
module Mstr = Tmodule.Mstr

type module_info = {
  sns_pred : lsymbol Mstr.t;  (** Representation predicates *)
  sns_id : vsymbol Mstr.t;  (** Indentifiers *)
  sns_ns : module_info Mstr.t;  (** Modules in scope *)
}

let empty_ns =
  { sns_pred = Mstr.empty; sns_id = Mstr.empty; sns_ns = Mstr.empty }

let ts_prop = mk_ts (Ident.create ~loc:Location.none "Hprop") []
let ty_prop = ty_app ts_prop []
let get_rep_pred = String.capitalize_ascii
let mk_update s = "_" ^ s ^ "'"
let mk_prog s = "_prog_" ^ s

let change_id map id =
  Ident.create ~attrs:id.id_attrs ~loc:id.id_loc (map id.id_str)

let is_present ns id =
  let str = id.id_str in
  Mstr.mem str ns.sns_id

let map_id ns is_old vs ty =
  let id =
    if (not is_old) && is_present ns vs.vs_name then
      change_id mk_update vs.vs_name
    else vs.vs_name
  in
  let val_vs = { vs_name = id; vs_ty = ty } in
  let new_map = Mstr.add id.id_str val_vs ns.sns_id in
  (val_vs, { ns with sns_id = new_map })

let get_id ns is_old id =
  let name = if is_old then id else mk_update id in
  try Mstr.find name ns.sns_id with Not_found -> Mstr.find id ns.sns_id

let create_rep_pred sym =
  let id = sym.ts_ident in
  let self_type =
    {
      ty_node =
        Tyapp (sym, List.map (fun x -> { ty_node = Tyvar x }) sym.ts_args);
    }
  in
  let model_type =
    match sym.ts_rep with Self -> self_type | Model (_, m) -> m
  in
  let new_id = change_id get_rep_pred id in
  {
    ls_name = new_id;
    ls_args = [ self_type; model_type ];
    ls_value = ty_bool;
    ls_constr = false;
    ls_field = false;
  }

let ty_ident ty =
  match ty.ty_node with Tyapp (ts, _) -> ts.ts_ident | _ -> assert false

let get_pred ns ty =
  match ty.ty_node with
  | Tyapp (ts, _) -> (
      try Mstr.find ts.ts_ident.id_str ns.sns_pred
      with Not_found -> create_rep_pred ts)
  | _ -> assert false

let id_of_term t =
  match t.Tterm.t_node with Tterm.Tvar v -> v | _ -> assert false

let get_ty_name ty =
  match ty.ty_node with
  | Tyapp (t, _) -> t.ts_ident.id_str
  | Tyvar v -> v.tv_name.id_str
