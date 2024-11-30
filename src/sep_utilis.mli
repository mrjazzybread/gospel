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

val change_id : (string -> string) -> Ident.t -> Ident.t
(** Returns a fresh identifier where the [id_str] field of [change_id f id] is
    equal to [f id.id_str] *)

val rep_pred : string -> string
(** Receives the name of a type and generates the name of its representation
    predicate *)

val match_field : lsymbol -> Ident.t * ty list * ty
(** When the argument is a [Field_symbol], returns the components of the
    constructor. This function is undefined for other values. *)
