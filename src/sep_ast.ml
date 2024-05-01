open Ppxlib
open Tterm
open Tast

type sep_term =
  | Star of sep_term list
  | Pure of term
  | App of Symbols.lsymbol * Symbols.vsymbol list
  | Magic of sep_term * sep_term
  | Forall of Symbols.vsymbol list * sep_term
  | Exists of Symbols.vsymbol list * sep_term
  | Lambda of Symbols.vsymbol list * sep_term
  | RO of sep_term
  | Top

type type_definition = { t_name : Ident.t; t_args : Ident.t list }

type triple = {
  triple_name : Ident.t;
  triple_args : Symbols.vsymbol option list;
  triple_vars : Symbols.vsymbol list;
  triple_pre : sep_term;
  triple_type : core_type;
  triple_post : sep_term;
}

type definition_node =
  | Pred of Ident.t * Symbols.vsymbol list
  | Type of Ident.t * Ttypes.tvsymbol list
  | Triple of triple
  | Axiom of Tast.axiom
  | Function of Tast.function_

type definition = { d_node : definition_node; d_loc : Location.t }
