(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(** Various utility functions to facilitate the translation of Gospel into
    Separation Logic *)

open Ttypes
open Symbols

type namespace
(** Maps strings to their corresponding representation predicates *)

type env
(** Maps strings to their corresponding program variables *)

val empty_module : namespace
(** The empty module *)

val empty_env : env
(** The empty environment *)

val get_pred : namespace -> Ttypes.ty -> Sast.psymbol option
(** [get_pred m s] returns the representation predicate for the type [s].
    @raise Not_found
      If [s] is a type whose logical model is isomorphic to its OCaml
      representation or [s] is not in scope. *)

val map_pred : namespace -> Ident.t -> ty list -> namespace
(** [map_pred m s l] returns a module [m] where [get_pred m s = l] *)

val change_id : (string -> string) -> Ident.t -> Ident.t
(** Returns a fresh identifier where the [id_str] field of [change_id f id] is
    equal to [f id.id_str] *)

val update_var : string -> string
(** To be used in conjunction with [change_id]. [update_var s] returns the name
    of the variable that represents the updated model of [s]. *)

val prog_var : string -> string
(** To be used in conjunction with [change_id]. [prog_var s] returns the program
    variable for [s] *)

val rep_pred : string -> string
(** Receives the name of a type and generates the name of its representation
    predicate *)

val unit_vs : vsymbol
(** Represents the value [()] *)

val match_field : lsymbol -> Ident.t * ty list * ty
(** When the argument is a [Field_symbol], returns the components of the
    constructor. This function is undefined for other values. *)

val is_pure_type : vsymbol -> bool
(** Checks if the type of the variable is mutable *)

val to_prog_type : Ttypes.ty -> Ttypes.ty
(** If the model of this type is a record then we return a type whose name is
    the same as the original, but with an '_' at the start. Otherwise, returns
    the type unchaged *)

val is_present : env -> Ident.t -> bool
(** Checks if the variable exists in the environment *)

val get_id : env -> string -> vsymbol
(** Gets the identifier mapped to the given variable name *)

val map_id : env -> bool -> vsymbol -> Ttypes.ty -> vsymbol * env
(** [map_id env is_old vs ty] returns a new [vsymbol] with the type [ty]. If the
    [is_old] flag is true, then nam of the returned variable is unchanged.
    Otherwise it is modified using [update_var]. The returned environemnt is
    [env] containing the returned variable *)

val ty_poly : Ttypes.ty list -> tvsymbol list
(** [ty_poly ty] Returns polymorphic variables in type [ty] *)

val function_poly : Tast.function_ -> tvsymbol list
(** [function_poly f] return a list with the polymorphic type variables of [f].
    This includes the type variables of its arguments and the type variables of
    existentially and universally quantified symbols within the definition of
    [f] *)
