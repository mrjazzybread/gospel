(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

open Uast

(* The following functions receive as their first arguments the list of type
   variables used within the structure they type check. *)

val axiom : Ident.t list -> IdUast.axiom -> Tast2.axiom
val function_ : Ident.t list -> IdUast.function_ -> Tast2.function_ * IdUast.pty

val spec :
  Ident.t list ->
  (* Type variables *)
  IdUast.sp_var list ->
  (* Arguments*)
  IdUast.sp_var list ->
  (* Return values *)
  IdUast.term list ->
  (* Pre conditions *)
  IdUast.term list ->
  (* Post conditions *)
  Tast2.term list * Tast2.term list

val invariant :
  Ident.t list -> Ident.t -> IdUast.pty -> IdUast.term -> Tast2.term
