open Ppxlib
open Tterm
open Identifier
open Tast

type sep_term = {
  s_node : sep_node;
  s_loc : Location.t;
} 

and sep_node = 
  |Points of Ident.t * Ident.t
  |Star of sep_term list
  |Pure of term
  |App of Ident.t * Ident.t list
  |Magic of sep_term * sep_term
  |Forall of Preid.t list * sep_term 
  |Exists of Preid.t list * sep_term 
  |Lambda of Preid.t * sep_term

type pred_field = {
  f_name : Ident.t;
  f_type : Ttypes.ty;
}

type type_definition = {
  t_name : Ident.t;
  t_args : Ident.t list;
}

type definition_node = 
  |Pred of Ident.t * pred_field list
  |Type of type_definition
  |Triple of sep_term * val_description * sep_term 

type definition = {
  d_node : definition_node;
  d_loc : Location.t;
}