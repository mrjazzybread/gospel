open Types

type tsymbol = { ts_id : Ident.t; ts_ty : Types.ty }
(** Typed variables *)

(** Typed variables *)
let mk_ts ts_id ts_ty = { ts_id; ts_ty }

(** Typed terms *)
type term_node =
  | Ttrue
  | Tfalse
  | Tnot of term
  | Tvar of Ident.t
  | Tlet of Ident.t * term * term
  | Tconst of Ppxlib.constant
  | Tapply of term * term
  | Tquant of Uast.IdUast.quant * tsymbol list * term
  | Tif of term * term * term

and term = { t_node : term_node; t_ty : ty; t_loc : Location.t }

let mk_term t_node t_ty t_loc = { t_node; t_ty; t_loc }

(* Typed Signatures *)

type axiom = {
  ax_name : Ident.t;  (** Name *)
  ax_term : term;  (** Definition *)
  ax_loc : Location.t;  (** Location *)
  ax_text : string;
      (** String containing the original specificaion as written by the user *)
}

type fun_spec = {
  fun_req : term list;  (** Preconditions *)
  fun_ens : term list;  (** Postconditions *)
  fun_variant : term list;  (** Variant *)
  fun_text : string;
      (** String containing the original specificaion as written by the user *)
  fun_loc : Location.t;  (** Specification location *)
}

type function_ = {
  fun_name : Ident.t;  (** Function symbol *)
  fun_rec : bool;  (** Recursive *)
  fun_params : tsymbol list;  (** Arguments *)
  fun_ret : ty;
  fun_def : term option;  (** Definition *)
  fun_spec : fun_spec option;  (** Specification *)
  fun_text : string;
      (** String containing the original specificaion as written by the user *)
  fun_loc : Location.t;  (** Location *)
}

and s_module_type_desc = Mod_signature of s_signature

and s_module_declaration = {
  mdname : Ident.t option;
  mdtype : s_module_type;
  mdattributes : Ppxlib.attributes;
  (* ... [@@id1] [@@id2] *)
  mdloc : Location.t;
}

and s_module_type = {
  mdesc : s_module_type_desc;
  mloc : Location.t;
  mattributes : Ppxlib.attributes; (* ... [@id1] [@id2] *)
}

and s_signature_item_desc =
  | Sig_function of function_
  | Sig_axiom of axiom
  | Sig_module of s_module_declaration

and s_signature_item = { sdesc : s_signature_item_desc; sloc : Location.t }
and s_signature = s_signature_item list

(* Helper functions *)

let mk_function f fun_params fun_def fun_ret fun_spec =
  {
    fun_name = f.Uast.IdUast.fun_name;
    fun_rec = f.fun_rec;
    fun_params;
    fun_def;
    fun_ret;
    fun_spec;
    fun_text = f.fun_text;
    fun_loc = f.fun_loc;
  }

let fun_to_arrow args ret =
  List.fold_right (fun arg ret -> Types.ty_arrow arg.ts_ty ret) args ret

let mk_axiom ax_name ax_term ax_loc ax_text =
  { ax_name; ax_term; ax_loc; ax_text }
