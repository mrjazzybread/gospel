(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

val x : 'a ref
val f1 : 'a -> unit
(*@ f1 y
    modifies x
    requires x = y
    ensures x = y *)

val f2 : unit -> 'a
(*@ y = f2 ()
    preserves x
    ensures y = x *)

val f3 : 'a -> unit
(*@ f3 y
    consumes x
    requires x = x
    ensures x = x *)

module M : sig
  type t
  (*@ mutable model : val *)

  val x : t
end

val f5 : M.t -> M.t
(*@ _ = f5 y
    preserves x
    ensures y = y *)

val f6 : unit -> M.t
(*@ x = f6 ()
    preserves M.x
    ensures M.x = x *)

val f8 : unit -> bool
(*@ b = f8 ()
    preserves M.x
    preserves x
    ensures b <-> x = M.x *)
