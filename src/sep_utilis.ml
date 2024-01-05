open Symbols
open Ttypes
open Ident
module Mstr = Tmodule.Mstr

type sep_namespace = {
    sns_pred : vsymbol Mstr.t;
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
               []
let ts_prop = mk_ts
                (Ident.create ~loc:Location.none "Prop")
                []

let ty_prop = ty_app ts_prop []
let ty_loc = ty_app ts_loc []

let rep_pred_pref = "@R_"
let get_rep_pred s = rep_pred_pref ^ s

let val_pref = "@V_"
let mk_val s = val_pref ^ s
let mk_update s =  mk_val (s ^ "'")

let change_id id map =
  Ident.create
    ~attrs:id.id_attrs
    ~loc:id.id_loc
    (map id.id_str)

let map_id ns vs is_old ty =
  let map = if is_old then mk_val else mk_update in 
  let id = change_id vs.vs_name map in
  let val_vs = {vs_name = id; vs_ty = ty} in
  let new_map = Mstr.add id.id_str val_vs ns.sns_id in
  {ns with sns_id = new_map}

let get_id ns is_old id =
  let map = if is_old then mk_val else mk_update in
  let name = map id in 
  try Mstr.find name ns.sns_id with
  |Not_found -> begin print_endline id; raise Not_found end

let get_pred ns ty_name =
  Mstr.find ty_name ns.sns_pred

let id_of_term t = match t.Tterm.t_node with
  |Tterm.Tvar v -> v
  |_ -> assert false

let get_ty_name ty = match ty.ty_node with
  |Tyapp (t, _) -> t.ts_ident.id_str
  |Tyvar v -> v.tv_name.id_str

