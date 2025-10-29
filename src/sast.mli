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
    Gospel *)

type psymbol = { ps_name : Ident.t; ps_args : Id_uast.pty list; ps_sep : bool }

type sep_terms = sep_term list
(** Conjunction of Separation Logic terms *)

(** Separation Logic terms *)
and sep_term =
  | Logical of Tast.term
  | Lift of psymbol * Tast.term * Tast.term
      (** Representation Predicate application *)
  | Wand of sep_terms * sep_terms
  | Quant of Parse_uast.quant * Tast.tsymbol list * sep_terms

type triple_val =
  | Unit
  | Wildcard
  | Ghost of Tast.tsymbol
  | Value of {
      arg_ocaml : Tast.tsymbol;
      arg_model : Tast.tsymbol;
      (* Note: the model is only relevant for arguments.  For return
         values, defer to the list of existentially quantified
         variables.*)
      is_loc : bool;
    }

type triple = {
  triple_name : Ident.t;  (** Name of the function. *)
  triple_poly : Ident.t list;  (** The function's type arguments. *)
  triple_args : triple_val list;  (** The function's arguments. *)
  triple_rets : triple_val list;  (** The return values for the function. *)
  triple_pre : sep_terms;
  triple_post : Tast.tsymbol list * sep_terms;
      (** The postcondtion terms and the existentially quantified variables. *)
}
(** Separation Logic Triple *)

(** Type Definitions *)
type tdef =
  | Abstract
  | Alias of Id_uast.pty
  | Record of (Ident.t * Id_uast.pty) list

type type_decl = {
  type_name : Ident.t;
  type_args : Ident.t list;
  type_ocaml : bool;
  type_def : tdef;
}
(** Type declarations *)

type axiom = {
  sax_name : Ident.t;
  sax_tvars : Ident.t list;
  sax_loc : Location.t; [@printer Utils.Fmt.pp_loc]
  sax_term : sep_terms;
}
(** Axioms *)

(** Top level definitions *)
type definition_node =
  | Pred of Tast.lens_info
  | Type of type_decl
  | Triple of triple
  | Val of Tast.s_val_description
  | Axiom of axiom
  | Function of Tast.function_
  | Module of Ident.t * definition list
  | Import of Id_uast.qualid

and definition = { d_node : definition_node; d_loc : Location.t }

type definitions = definition list
