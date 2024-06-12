open Ppxlib
open Tterm
open Tast

(** Separation Logic terms *)
type sep_term =
  | Pure of term  (** Pure term *)
  | App of Symbols.lsymbol * term list
      (** Representation predicate application, *)

type triple = {
  triple_name : Ident.t;  (** function name *)
  triple_args : Symbols.vsymbol option list;
      (** triple_arguments. None when they are unamed (i.e. ()) *)
  triple_vars : Symbols.vsymbol list;  (** Universally quantified variables *)
  triple_rets : Symbols.vsymbol list;  (** Return values *)
  triple_pre : sep_term list;
      (** Precondition terms connected with the separating conjuction*)
  triple_type : core_type;  (** Function type *)
  triple_post : Symbols.vsymbol list * sep_term list;
      (** Postcondition existentially quantified variables and terms connected
          with the separating conjuction *)
}

(** Top level definitions *)
type definition_node =
  | Pred of Ident.t * Symbols.vsymbol list  (** Representation Predicate *)
  | Type of Ident.t * Ttypes.tvsymbol list  (** Type definition *)
  | Triple of triple  (** Separation Logic Triples *)
  | Axiom of Tast.axiom  (** Axiom *)
  | Function of Tast.function_  (** Logical Function *)
  | Module of Ident.t * definition list

and definition = { d_node : definition_node; d_loc : Location.t }
