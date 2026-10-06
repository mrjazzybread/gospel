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

val change_id : (string -> string) -> Ident.t -> Ident.t
(** Returns a fresh identifier where the [id_str] field of [change_id f id] is
    equal to [f id.id_str] *)

val inline_def : Sast.triple -> Sast.triple
(** [inline_def t] inlines, when possible, the existential variables in the
    postcondition of the triple [t]. This is done when the postcondition is a
    term with the following shape: [exists m'. P(e1, m') * ... * m' = e2 * ...]
    which is then simplified into [P(e1, e2)] *)
