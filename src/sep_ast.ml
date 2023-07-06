open Ppxlib
open Tterm
open Tast

type sep_term = {
  s_node : sep_node;
  s_loc : Location.t;
} 

and sep_node = 
  |Star of sep_term list
  |Pure of term
  |App of Symbols.vsymbol * Symbols.vsymbol list
  |Magic of sep_term * sep_term
  |Forall of Symbols.vsymbol list * sep_term 
  |Exists of Symbols.vsymbol list * sep_term 
  |Lambda of Symbols.vsymbol list * sep_term
  |RO of sep_term
  |Top

let mk_sep_term ?(l = Location.none) t = {s_node = t; s_loc = l}

type type_definition = {
  t_name : Ident.t;
  t_args : Ident.t list;
}

type triple = {
  triple_name : Ident.t;
  triple_args : Symbols.vsymbol list;
  triple_pre : sep_term;
  triple_type : core_type;
  triple_post : sep_term;
}

type definition_node = 
  |Pred of Ident.t * Symbols.vsymbol list
  |Type of Ident.t
  |Triple of triple

type definition = {
  d_node : definition_node;
  d_loc : Location.t;
}


