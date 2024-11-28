(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(** Defines a Separation Logic language that used to define the semantics of
    Gospel*)

open Ppxlib
open Tast

type psymbol = { ps_name : Ident.t; ps_args : Ttypes.ty list }

type sep_terms = sep_term list
(** Conjunction of Separation Logic terms *)

(** Separation Logic terms *)
and sep_term =
  | Pure of Tterm.term
  | Lift of psymbol * Tterm.term list
      (** Representation Predicate application*)
  | Wand of sep_terms * sep_terms
  | Quant of Tterm.quant * Symbols.vsymbol list * sep_terms

type triple = {
  triple_name : Ident.t;  (** Name of the function *)
  triple_poly : Ttypes.tvsymbol list;  (** The function's type arguments*)
  triple_args : Symbols.vsymbol list;  (** The function's arguments *)
  triple_vars : Symbols.vsymbol list;
      (** All the universally quantified variables in the specification.
          Includes all the variables within [triple_args]*)
  triple_rets : Symbols.vsymbol list;  (** The return values for the function *)
  triple_checks : Tterm.term list;
  triple_pre : sep_terms;
  triple_type : core_type;
  triple_post : Symbols.vsymbol list * sep_terms;
      (** The postcondtion terms and the existentially quantified variables. *)
}
(** Separation Logic Triple *)

type ptype = Persistent | Heap

type rep_pred = {
  pred_name : Ident.t;
  pred_poly : Ttypes.tvsymbol list;
  pred_args : Symbols.vsymbol list;
  pred_type : ptype;
}
(** Representation Predicates *)

(** Type Definitions *)
type tdef = Abstract | Record of (Ident.t * Ttypes.ty) list

type type_decl = {
  type_name : Ident.t;
  type_args : Ttypes.tvsymbol list;
  type_def : tdef;
}
(** Type declarations *)

type axiom = {
  sax_name : Ident.t;
  sax_loc : Location.t; [@printer Utils.Fmt.pp_loc]
  sax_term : sep_terms;
}
(** Axioms *)

(** Top level definitions *)
type definition_node =
  | Pred of rep_pred
  | Type of type_decl
  | Triple of triple
  | Axiom of Ttypes.tvsymbol list * axiom
  | Function of Ttypes.tvsymbol list * Tast.function_
  | Module of Ident.t * definition list
  | Import of string list

and definition = { d_node : definition_node; d_loc : Location.t }
