open Uast
open Inferno
open Identifier

module X = struct
  (* The module X (following the naming convention of Solver.Make)
     provides a type of variables that will be assigned polymorphic
     schemes by the solver.

     In a toy ML language, the syntact constructs that introduce
     polymorphic schemes always correspond to binding constructs for
     term variables: (let x = t in u) in particular, or (fun x -> t)
     which can be considered to give a "trivial" (non-polymorphic)
     scheme to x.

     In our richer language, we also compute polymorphic schemes for
     type annotations containing rigid variables:

       (t : for 'a 'b. 'a * 'b -> 'a)

     For this construct there is no term variable associated to the
     scheme, instead we create a "symbolic" variable (a fresh integer)
     to pass to the solver.
   *)
  type t = qualid

  let hash = Hashtbl.hash

  let compare v1 v2 = Stdlib.compare v1 v2

  let equal v1 v2 = compare v1 v2 = 0

  let rec to_string = function
    | Qpreid id -> id.pid_str
    | Qdot (q, id) -> to_string q ^ id.pid_str

end

module S = struct

  type 'a structure =
    | TyApp of string * 'a list

  let ty_bool = TyApp("bool", [])
  let ty_int = TyApp("int", [])

  let iter f t =
    match t with
    | TyApp (_, l) ->
       List.iter f l

  let fold f t accu =
    match t with
    | TyApp (_, l) ->
       let accu = List.fold_left (fun x y -> f y x) accu l in
       accu

  let map f t =
    match t with
    | TyApp (id, l) ->
       TyApp (id, List.map f l)

(* -------------------------------------------------------------------------- *)

(* Traversals at arity 2. *)

exception Iter2

let list_iter2 f ts us =
  if List.length ts <> List.length us then raise Iter2;
  List.iter2 f ts us

let iter2 f t u =
  match t, u with
  | TyApp (_, l1), TyApp (_, l2) ->
      List.iter2 f l1 l2;

(* The function [conjunction] that is expected by the solver is essentially
   [iter2], except it must return a structure, as opposed to a unit value. *)

exception InconsistentConjunction =
  Iter2

let conjunction f t u =
  iter2 f t u;
  t

(* -------------------------------------------------------------------------- *)

(* Printing. *)

open PPrint

let pprint leaf s =
  match s with
  | TyApp (id, l) ->
      string id ^^ List.fold_left (fun acc x -> acc ^^ leaf x) empty l
  
end

module O = struct

  type tyvar = Ttypes.tvsymbol

  (* See also [fresh_tyvar] *)
  let inject n = { Ttypes.tv_name = Ident.create ~loc:Location.none ("a" ^ string_of_int n) }
  type 'a structure = 'a S.structure
  let pprint = S.pprint

  type ty = Ttypes.ty

  let variable x = { Ttypes.ty_node = Tyvar x }

  let structure t =
    match t with
    | S.TyApp (id, l) ->
        { Ttypes.ty_node = Tyapp(Ttypes.ts (Ident.create ~loc:Location.none id) [], l) }

  let mu _ _ = assert false

end

module Solver = Solver.Make(X)(S)(O)
open Solver

type tterm_node =
  | Ttrue
  | Tfalse
  | Tint of int
  | Tsub of tterm * tterm
  | Tand of tterm * tterm
  | Tvar of string
  | Tlet of string * tterm * tterm

and tterm = { t : tterm_node; ty : O.ty }

let mk_term t ty = {t; ty}

let rec hastype (t : Uast.term) (w : variable) =
  match t.term_desc with
  | Uast.Ttrue ->
     let+ () = w --- S.ty_bool in
     mk_term Ttrue Ttypes.ty_bool
  | Uast.Tfalse ->
     let+ () = w --- S.ty_bool in
     mk_term Tfalse Ttypes.ty_bool
  | Uast.Tinfix(t1, op, t2) ->
     if op.pid_str = "infix -" then begin
         let+ tt1 = lift hastype t1 S.ty_int 
         and+ tt2 = lift hastype t2 S.ty_int in
         mk_term (Tsub(tt1, tt2)) Ttypes.ty_int
       end
     else
       assert false
  | Uast.Tbinop(t1, Tand, t2) ->
     let+ tt1 = lift hastype t1 S.ty_bool
     and+ tt2 = lift hastype t2 S.ty_bool in
     mk_term (Tand(tt1, tt2)) Ttypes.ty_bool
  | Uast.Tconst constant ->
     begin match constant with
     | Pconst_integer (s, _) ->
        let+ () = w --- S.ty_int in
        mk_term (Tint (int_of_string s)) Ttypes.ty_int
     | _ -> assert false end
  | Uast.Tpreid q ->
     let+ tys = instance q w in
     let ty = match tys with
     |[ty] -> ty
     | _ -> assert false in
     mk_term (Tvar (X.to_string q)) ty
  | Uast.Tlet(id, t1, t2) ->
     let+ (_, (_, _), tt1, tt2)  = let1 (Qpreid id) (hastype t1) (hastype t2 w) in
     mk_term (Tlet(id.pid_str, tt1, tt2)) tt2.ty
  | _ -> assert false

let typecheck t =
  let@ w = exist in
  hastype t w
