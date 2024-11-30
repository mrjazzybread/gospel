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

val empty_module : namespace
(** The empty module *)

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

val rep_pred : string -> string
(** Receives the name of a type and generates the name of its representation
    predicate *)

val match_field : lsymbol -> Ident.t * ty list * ty
(** When the argument is a [Field_symbol], returns the components of the
    constructor. This function is undefined for other values. *)

val function_poly : Tast.function_ -> tvsymbol list
(** [function_poly f] return a list with the polymorphic type variables of [f].
    This includes the type variables of its arguments and the type variables of
    existentially and universally quantified symbols within the definition of
    [f] *)
