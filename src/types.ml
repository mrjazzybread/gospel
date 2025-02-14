(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

open Uast.IdUast
module S = Structure

type tyvar = int

let leaf q = match q with Qid id -> id | Qdot (_, id) -> id
let inject n = n

type 'a structure = 'a S.structure

let pprint = S.pprint

type ty = Uast.IdUast.pty

(** Maps a type variable to a decoded type*)
let variable _ = PTtyvar (Ident.tvar ())

(** Maps a structure whose variables are decoded types into a decoded type *)
let structure t =
  match t with
  | S.Tyapp (id, l) -> PTtyapp (Qid id, l)
  | Tyarrow (t1, t2) -> PTarrow (Lunit, t1, t2)

(** Since Gospel types are not allowed to be cyclic, we do not need to define
    the mu funciton *)
let mu _ _ = assert false

open Utils.Fmt

let ty_arrow arg ret = PTarrow (Lunit, arg, ret)
let ty_bool = PTtyapp (Qid S.bool_id, [])

let rec print_tv fmt tv = pp fmt "%d" tv
and print_arrow_ty fmt = list ~sep:arrow print_ty fmt

and print_ty fmt = function
  | PTtyvar v -> pp fmt "%a" print_tv v.id_tag
  | PTarrow (_, (PTarrow _ as ty1), ty2) ->
      pp fmt "(%a)%a%a" print_ty ty1 arrow () print_ty ty2
  | PTarrow (_, ty1, ty2) -> print_arrow_ty fmt [ ty1; ty2 ]
  | PTtyapp (ts, []) -> pp fmt "@[%s@]" (leaf ts).id_str
  | PTtyapp (ts, [ ty ]) -> pp fmt "%a %s" print_ty ty (leaf ts).id_str
  | PTtyapp (ts, tyl) ->
      pp fmt "(%a) %s" (list ~sep:comma print_ty) tyl (leaf ts).id_str
  | PTtuple _ -> assert false
