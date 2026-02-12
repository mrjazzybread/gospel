(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(* The following functions receive as their first arguments the list
   of type variables used within the structure they type check.  This
   is not strictly necessary for type inference, however when Inferno
   generates fresh type variables, we have to give them names.  In
   order to ensures that the names of these variables do not clash
   with the variables the user might have written in the Gospel
   structure, we require the clients of this library to pass them as
   argument.

   Note: The type variables used within the structure could be
   collected by this module, however, the [Typing] module already does
   this in order to do several type checking tasks. *)

val axiom : Ident.t list -> Id_uast.axiom -> Tast.axiom

val function_ :
  Ident.t list -> Id_uast.function_ -> Tast.function_ * Id_uast.pty

val spec : Ident.t list -> Id_uast.val_spec -> Tast.val_spec * Tast.tvar list

val invariant :
  Ident.t list -> Ident.t -> Id_uast.pty -> Id_uast.term -> Tast.term

val ty_inst : Id_uast.pty -> Ident.t list -> Id_uast.pty list -> Id_uast.pty
(** [ty_inst ty vars args] instantiates the type variables in [ty] with the type
    arguments [args]. This function always succeeds as long as the length of
    [vars] and [args] is equal. Additionally, the resulting type will not
    generate any new type variables that were not already present in [args]. *)

val apply_lens : Id_uast.pty -> Id_uast.lens -> Id_uast.pty
