(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(** This file contains the necessary definitions to create an Inferno Solver.
    This includes the definitions of program variables, types with undefined
    type variables and decoded (i.e. fully resolved) types. *)

open Identifier

(** Definition of program variables. *)
module UniqueId = struct
  type t = {
    id_str : string; (* Variable name. Not used internally. *)
    id_attrs : string list; (* Variable attributes *)
    id_loc : Location.t;
    id_tag : int;
        (* Unique identifier. During typechecking, this is what
           Inferno uses to check if two variables are the same. *)
  }

  let to_string id = id.id_str
  let compare x y = Int.compare x.id_tag y.id_tag
  let equal x y = x.id_tag = y.id_tag
  let hash x = x.id_tag

  let gen_tag =
    let r = ref 0 in
    fun () ->
      r := !r + 1;
      !r

  let mk_id id_str id_attrs id_loc =
    { id_str; id_attrs; id_loc; id_tag = gen_tag () }

  let from_preid p =
    {
      id_str = p.Preid.pid_str;
      id_attrs = p.pid_attrs;
      id_loc = p.pid_loc;
      id_tag = gen_tag ();
    }
end

(** Types with undefined type variables. *)
module S = struct
  (** Represents a Gospel type where the type variables are represented by
      values of type ['a]. *)
  type 'a structure = Tyapp of UniqueId.t * 'a list

  let create_id s = UniqueId.mk_id s [] Location.none

  (* Built in Gospel types *)
  let bool_id = create_id "bool"
  let integer_id = create_id "integer"
  let char_id = create_id "char"
  let string_id = create_id "string"
  let float_id = create_id "float"
  let sequence_id = create_id "sequence"
  let arrow_id = create_id "->"
  let ty_bool = Tyapp (bool_id, [])
  let ty_integer = Tyapp (integer_id, [])
  let ty_char = Tyapp (char_id, [])
  let ty_string = Tyapp (string_id, [])
  let ty_float = Tyapp (float_id, [])

  let primitive_list =
    [
      ("bool", bool_id);
      ("integer", integer_id);
      ("char", char_id);
      ("string", string_id);
      ("float", float_id);
      ("sequence", sequence_id);
    ]

  let ty_arrow v1 v2 = Tyapp (arrow_id, [ v1; v2 ])

  (* Traversal functions required by Inferno *)
  let iter f (Tyapp (_, args)) = List.iter f args

  let fold f (Tyapp (_, args)) accu =
    List.fold_left (fun x y -> f y x) accu args

  let map f (Tyapp (id, args)) = Tyapp (id, List.map f args)

  (* -------------------------------------------------------------------------- *)

  (* Traversals at arity 2. *)

  exception Iter2

  let list_iter2 f ts us =
    if List.length ts <> List.length us then raise Iter2;
    List.iter2 f ts us

  let iter2 f (Tyapp (id1, args1)) (Tyapp (id2, args2)) =
    if not (UniqueId.equal id1 id2) then raise Iter2
    else list_iter2 f args1 args2

  exception InconsistentConjunction = Iter2

  (* The function [conjunction] that is expected by the solver is essentially
   [iter2], except it must return a structure, as opposed to a unit value. *)
  let conjunction f t u =
    iter2 f t u;
    t

  (* -------------------------------------------------------------------------- *)

  (* Printing. *)

  (* open PPrint *)

  let pprint _ _ = assert false
end

(** Definition of types where every type variable has been decoded *)
module O = struct
  type tyvar = int

  let inject n = n

  type 'a structure = 'a S.structure

  let pprint = S.pprint

  type ty =
    | Tyvar of tyvar  (** Type variables (e.g. 'a, 'b). *)
    | Tyapp of UniqueId.t * ty list  (** Decoded types.*)

  (** Maps a type variable to a decoded type*)
  let variable x = Tyvar x

  (** Maps a structure whose variables are decoded types into a decoded type *)
  let structure t = match t with S.Tyapp (id, l) -> Tyapp (id, l)

  (** Since Gospel types are not allowed to be cyclic, we do not need to define
      the mu funciton *)
  let mu _ _ = assert false

  open Utils.Fmt

  let is_id_arrow = UniqueId.equal S.arrow_id

  let rec print_tv fmt tv = pp fmt "%d" tv
  and print_arrow_ty fmt = list ~sep:arrow print_ty fmt

  and print_ty fmt = function
    | Tyvar v -> pp fmt "%a" print_tv v
    | Tyapp (ts, []) -> pp fmt "%s" ts.id_str
    | Tyapp (ts, tys) when is_id_arrow ts -> print_arrow_ty fmt tys
    | Tyapp (ts, [ ty ]) -> pp fmt "%a %s" print_ty ty ts.id_str
    | Tyapp (ts, tyl) ->
        pp fmt "(%a) %s" (list ~sep:comma print_ty) tyl ts.id_str
end
