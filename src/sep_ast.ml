open Ppxlib
open Tast

(** Separation Logic terms *)
type sep_term =
  | Pure of Tterm.term  (** Pure term *)
  | App of Symbols.lsymbol * Tterm.term list
      (** Representation predicate application, *)

type triple = {
  triple_name : Ident.t;  (** function name *)
  triple_poly : Ttypes.tvsymbol list;  (** polymorphic variable names *)
  triple_args : Symbols.vsymbol list;
      (** triple_arguments. None when they are unamed (i.e. ()) *)
  triple_vars : Symbols.vsymbol list;  (** Universally quantified variables *)
  triple_rets : Symbols.vsymbol list;  (** Return values *)
  triple_checks : Tterm.term list;
  triple_pre : sep_term list;
      (** Precondition terms connected with the separating conjuction*)
  triple_type : core_type;  (** Function type *)
  triple_post : Symbols.vsymbol list * sep_term list;
      (** Postcondition existentially quantified variables and terms connected
          with the separating conjuction *)
}

type rep_pred = {
  pred_name : Ident.t;
  pred_poly : Ttypes.tvsymbol list;
  pred_args : Symbols.vsymbol list;
}

type type_def = {
  type_name : Ident.t;
  type_args : Ttypes.tvsymbol list;
  type_mut : bool;
}

(** Top level definitions *)
type definition_node =
  | Pred of rep_pred  (** Representation Predicate *)
  | Type of type_def  (** Type definition *)
  | Triple of triple  (** Separation Logic Triples *)
  | Axiom of Ttypes.tvsymbol list * Tast.axiom  (** Axiom *)
  | Function of Ttypes.tvsymbol list * Tast.function_  (** Logical Function *)
  | Module of Ident.t * definition list
  | Import of string list

and definition = { d_node : definition_node; d_loc : Location.t }
