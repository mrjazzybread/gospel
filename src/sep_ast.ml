open Ppxlib
open Tterm
open Tast

(** Separation Logic terms *)
type sep_term =
  | Star of sep_term list (** Separating conjuction *)
  | Pure of term (** Pure term *)
  | App of Symbols.lsymbol * Symbols.vsymbol list (** Representation predicate application *)
  | Exists of Symbols.vsymbol list * sep_term (** Existential quantifier *)
  | Top (** T *)
  | Magic of sep_term * sep_term (** Magic Wand, unused for now *)
  | RO of sep_term (** Read only permission, unused for now *)

type triple = {
    triple_name : Ident.t;
    (** function name *)
    triple_args : Symbols.vsymbol option list;
    (** triple_arguments. None when they are unamed (i.e. ()) *)
    triple_vars : Symbols.vsymbol list;
    (** Universally quantified variables *)
    triple_rets : Symbols.vsymbol list;
    (** Return values *)
    triple_pre : sep_term;
    (** Precondition *)
    triple_type : core_type;
    (** Function type *)
    triple_post : sep_term;
    (** Postcondition *)
}
(** Top level definitions *)
type definition_node =
  | Pred of Ident.t * Symbols.vsymbol list (** Representation Predicate *)
  | Type of Ident.t * Ttypes.tvsymbol list (** Type definition *)
  | Triple of triple (** Separation Logic Triples *)
  | Axiom of Tast.axiom (** Axiom *)
  | Function of Tast.function_ (** Logical Function *)

type definition = { d_node : definition_node; d_loc : Location.t }
