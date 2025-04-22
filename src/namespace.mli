(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(** This module provides operations to keep track of all names defined in the
    top level. *)

open Uast

type env
(** The (immutable) environment that keeps track of the definitions in the top
    level. *)

type mod_defs
(** A set of top level definitions *)

val scope : env -> mod_defs
(** Gets the set of definitions in scope *)

val defs : env -> mod_defs
(** Gets the set of definitions contained within the submodule we are currently
    processing. *)

(* Records containing the information necessary regarding each top level
    definitions *)
type mod_info = { mid : Ident.t; mdefs : mod_defs }

type record_info = {
  rid : Ident.t; (* The name of the record type. *)
  rparams : Ident.t list;
  (* The type parameters for the record type. Should be equal to the [tparams]
       list in the entry for [rid] in the corresponding type environment. *)
  rfields : (Ident.t * IdUast.pty) list; (* The list of all record fields. *)
}
(** [record_info] contains the necessary to type check field accesses and record
    creation. When a record type is processed, an entry of [record_info] is
    added as well as an entry of [type_info]. *)

(* Functions to update the environment by adding a top level definition *)
val add_fun : env -> Ident.t -> IdUast.pty -> env
val add_gospel_type : env -> Ident.t -> Ident.t list -> IdUast.pty option -> env

val add_ocaml_type :
  env ->
  Ident.t ->
  Ident.t list ->
  IdUast.pty option ->
  IdUast.pty option ->
  env
(** [add_ocaml_type env id params alias model] Adds the OCaml type [id] to the
    environment [env]. We associate with it the names of its type variables
    [params] and its type expansion [alias], if it exists. Additionally, we
    create a Gospel lens that claims ownership of values of type [id]. If
    [model] is not [None], then the created lens also allows values of type [id]
    to be used as values of type [model]. *)

val add_record :
  env -> Ident.t -> Ident.t list -> (Ident.t * IdUast.pty) list -> env

val add_exn : env -> Ident.t -> IdUast.pty list -> env
(** [add_exn env id args] adds an exception named [id] to [env] that receives
    arguments of type [args].

    Note: If [alias] is [Some e], [args] should be the same argument list as
    [e]. *)

val add_mod : env -> Ident.t -> mod_defs -> env

val resolve_application :
  ocaml:bool ->
  mod_defs ->
  ParseUast.qualid ->
  IdUast.pty list ->
  IdUast.app_info
(** If the type name [q] in an alias such as

    [type (tv_1, tv_2, ...) q = alias]

    [resolve_application ~ocaml env q l], where [l] are the type parameters of
    [q]. returns a type application where the [app_alias] field is the
    expression [Some alias] where each occurrence of [tv_i] has been replaced
    with [List.nth l i]. If [q] is not an alias the [app_alias] field is [None].

    Similarly, if [q] and each type parameter in [l] has a logical
    representation [model], [app_model] is set to [model] where every occurrence
    [tv_i] has been replaced with the logical representation of [List.nth l i].
    Otherwise, [None].

    The [ocaml] flag is used to indicate which namespace should be searched.

    This function will raise a Gospel exception if the size of list [l] is not
    equal to the amount of type parameters this type receives.

    Postcondition: If [ocaml] is [false], [app_model] will always be [None]. *)

val fun_qualid :
  mod_defs -> ParseUast.qualid -> IdUast.qualid * Ident.t list * IdUast.pty
(** [fun_qualid defs q] turns every sub identifier in [q] into a fully resolved
    function identifier. Also returns the function's type and the type
    parameters used.
    @raise Not_found if [q] is not a valid function identifier *)

val fields_qualid :
  loc:Location.t ->
  mod_defs ->
  ParseUast.qualid list ->
  (IdUast.qualid * IdUast.pty) list * IdUast.ty_app
(** [fields_qualid defs ~loc l] receives a list of identifiers used to create a
    record and returns:
    - A list of qualified identifiers for each record label in [l] coupled with
      their types.
    - The identifier for the record type the labels [l] belong to.
    - The type parameters for the record type. *)

val get_field_info :
  mod_defs -> ParseUast.qualid -> IdUast.qualid * IdUast.pty * IdUast.ty_app
(** [get_field_info defs id] receives a record field identifier and returns:
    - The resolved identifier for [q].
    - The type of the record field [q].
    - The identifier for the record type the label [id] belong to.
    - The type parameters for the record type. *)

val get_exn_info :
  mod_defs -> ParseUast.qualid -> IdUast.qualid * IdUast.pty list
(** [get_exn_info defs id] receives an exception identifier [id] and the types
    of its arguments. *)

val gospel_open : env -> ParseUast.qualid -> IdUast.qualid * env
(** [gospel_open defs id] adds the definitions in module [id] into the scope
    [defs]. *)

val local_open : mod_defs -> ParseUast.qualid -> IdUast.qualid * mod_defs
(** [local_open defs id] is similar to [gospel_open] but is applied to a local
    scope within a term and returns a new local scope with all the definitions
    in module [id]. *)

val empty_env : env
(** The empty environment. The only names in scope are primitive Gospel types.
    This should be the initial environment when processing the Gospel standard
    library. *)

val unit_id : Ident.t
(** The unique identifier for the unit type. *)

val init_env : ?ocamlprimitives:mod_defs -> mod_defs -> env
(** [init_env stdlib] receives the definitions in the Gospel standard library
    [stdlib] and creates an environment where every name in [stdlib] is in scope
    as well as every name in [empty_env]. Additionally creates a module named
    [Gospelstdlib] with every definition in [stdlib]. This should be the initial
    environment for any Gospel project. *)

val submodule : env -> env
(** Returns a new environment for processing a submodule. The variables in scope
    remain the same and the set of definitions is empty. *)
