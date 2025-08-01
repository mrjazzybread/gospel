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

type namespace
(** Maps strings to their corresponding representation predicates *)

val empty_env : unit -> namespace
(** The empty namespace *)

val get_pred : namespace -> Id_uast.pty -> Sast.psymbol option
(** [get_pred m s] returns the representation predicate for the type [s].
    @raise Not_found
      If [s] is a type whose logical model is isomorphic to its OCaml
      representation or [s] is not in scope. *)

val map_pred :
  namespace -> Ident.Tag.t -> Ident.t -> bool -> Id_uast.pty list -> unit
(** [map_pred m tag s is_mutable l] maps [tag] to a representation predicate
    named [s] that takes arguments of type [l]. *)

val change_id : (string -> string) -> Ident.t -> Ident.t
(** Returns a fresh identifier where the [id_str] field of [change_id f id] is
    equal to [f id.id_str] *)

val rep_pred : string -> string
(** Receives the name of a type and generates the name of its representation
    predicate *)
