open Ppxlib
open Tast

type sep_terms = sep_term list
(** Conjunction of Separation Logic terms *)

(** Separation Logic terms *)
and sep_term =
  | Pure of Tterm.term  (** Pure term *)
  | Lift of Symbols.lsymbol * Tterm.term list
      (** Representation predicate application *)
  | Wand of sep_terms * sep_terms  (** Magic wand *)
  | Quant of Tterm.quant * Symbols.vsymbol list * sep_terms
      (** Quantification of variables *)

type triple = {
  triple_name : Ident.t;  (** function name *)
  triple_poly : Ttypes.tvsymbol list;  (** polymorphic variable names *)
  triple_args : Symbols.vsymbol list;
      (** triple_arguments. None when they are unamed (i.e. ()) *)
  triple_vars : Symbols.vsymbol list;  (** Universally quantified variables *)
  triple_rets : Symbols.vsymbol list;  (** Return values *)
  triple_checks : Tterm.term list;
  triple_pre : sep_terms;
      (** Precondition terms connected with the separating conjuction*)
  triple_type : core_type;  (** Function type *)
  triple_post : Symbols.vsymbol list * sep_terms;
      (** Postcondition existentially quantified variables and terms connected
          with the separating conjuction *)
}

type rep_pred = {
  pred_name : Ident.t;
  pred_poly : Ttypes.tvsymbol list;
  pred_args : Symbols.vsymbol list;
}

type tdef = Abstract | Record of (Ident.t * Ttypes.ty) list

type type_decl = {
  type_name : Ident.t;
  type_args : Ttypes.tvsymbol list;
  type_def : tdef;
}

type axiom = {
  sax_name : Ident.t;  (** Name *)
  sax_loc : Location.t; [@printer Utils.Fmt.pp_loc]  (** Location *)
  sax_term : sep_terms;  (** Definition *)
}

type pure_axiom = {
  pax_name : Ident.t;
  pax_loc : Location.t;
  pax_term : Tterm.term;
}

(** Top level definitions *)
type definition_node =
  | Pred of rep_pred  (** Representation Predicate *)
  | Type of type_decl  (** Type definition *)
  | Triple of triple  (** Separation Logic Triples *)
  | Axiom of Ttypes.tvsymbol list * axiom  (** Axiom *)
  | Function of Ttypes.tvsymbol list * Tast.function_  (** Logical Function *)
  | Module of Ident.t * definition list
  | Import of string list

and definition = { d_node : definition_node; d_loc : Location.t }
