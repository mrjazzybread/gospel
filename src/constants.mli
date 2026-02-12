(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(** Identifiers *)

val bool_id : Ident.t
val integer_id : Ident.t
val char_id : Ident.t
val string_id : Ident.t
val float_id : Ident.t
val prop_id : Ident.t
val set_id : Ident.t
val val_id : Ident.t
val unit_id : Ident.t
val val_lens_id : Ident.t
val unit_lens_id : Ident.t

val primitive_list : (string * Ident.t) list
(** List of primitive Gospel types. *)

(** Types *)

val ty_prop : Id_uast.pty
val ty_val : Id_uast.pty
val ty_unit : Id_uast.pty

(** Lenses *)

val lens_unit : Id_uast.lens
val lens_val : Id_uast.lens
