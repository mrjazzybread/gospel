open Uast
open Inferno

module X = struct

  type t = qualid

  let rec to_string = function
    | Qpreid id -> id.pid_str
    | Qdot (q, id) -> to_string q ^ "." ^ id.pid_str

  let hash v = Hashtbl.hash (to_string v)

  let compare v1 v2 = 
    Stdlib.compare (to_string v1) (to_string v2)

  let equal v1 v2 = compare v1 v2 = 0
end

module S = struct

  type 'a structure =
    |Int
    |Bool
    |List of 'a


  let ty_bool = Bool
  let ty_int = Int
  let ty_list ty = List ty

  let iter f t =
    match t with
    | List x ->
       f x
    | _ -> ()

  let fold f t accu =
    match t with
    | List x ->
       f x accu
    |_ -> accu

  let map f t =
    match t with
    | List x -> List (f x)
    | Int -> Int
    | Bool -> Bool

(* -------------------------------------------------------------------------- *)

(* Traversals at arity 2. *)

exception Iter2

let list_iter2 f ts us =
  if List.length ts <> List.length us then raise Iter2;
  List.iter2 f ts us

let iter2 f t u =
  match t, u with
  | List x, List y-> 
     f x y
  | Int, Int | Bool, Bool -> ()
  | _ -> raise Iter2

(* The function [conjunction] that is expected by the solver is essentially
   [iter2], except it must return a structure, as opposed to a unit value. *)

exception InconsistentConjunction =
  Iter2

let conjunction f t u =
  iter2 f t u;
  t

(* -------------------------------------------------------------------------- *)

(* Printing. *)

(* open PPrint *)

let pprint _ _ = assert false
end

module O = struct

  type tyvar = string

  (* See also [fresh_tyvar] *)
  let inject n = "a" ^ string_of_int n

  type 'a structure = 'a S.structure

  let pprint = S.pprint

  type ty =
    |Var of string
    |Int
    |Bool
    |List of ty

  let ty_bool = Bool
  let ty_int = Int
  let ty_list x = List x

  let variable x = Var x

  let structure t =
    match t with
    | S.Int -> Int
    | S.Bool -> Bool
    | S.List v -> List v

  let mu _ _ = assert false

end

module Solver = Solver.Make(X)(S)(O)
open Solver

type tterm_node =
  | Ttrue
  | Tfalse
  | Tnil
  | Tint of int
  | Tcons of tterm * tterm
  | Tsub of tterm * tterm
  | Tand of tterm * tterm
  | Tvar of string
  | Tlet of string * tterm * tterm

and tterm = { t : tterm_node; ty : O.ty }

let mk_term t ty = {t; ty}

type error =
  | Unbound of string

exception Error of error

let rec leaf id =
  match id with
  |Qpreid id -> id
  |Qdot(q, _) -> leaf q

let rec hastype (t : Uast.term) (w : variable) =
  match t.term_desc with
  | Uast.Ttrue ->
     let+ () = w --- S.ty_bool in
     mk_term Ttrue O.ty_bool
  | Uast.Tfalse ->
     let+ () = w --- S.ty_bool in
     mk_term Tfalse O.ty_bool
  | Uast.Tconst constant ->
     begin match constant with
     | Pconst_integer (s, _) ->
        let+ () = w --- S.ty_int in
        mk_term (Tint (int_of_string s)) O.ty_int
     | _ -> assert false end
  | Uast.Tidapp(id, [t1; t2]) ->
     let id = leaf id in
     if id.pid_str = "infix -" then
       let+ tt1 = lift hastype t1 S.ty_int
       and+ tt2 = lift hastype t2 S.ty_int
       and+ () = w --- S.ty_int in
       mk_term (Tsub(tt1, tt2)) O.ty_int
     else
       assert false
  | Uast.Tbinop(t1, Uast.Tand, t2) ->
     let+ tt1 = lift hastype t1 S.ty_bool
     and+ tt2 = lift hastype t2 S.ty_bool in
     mk_term (Tand(tt1, tt2)) O.ty_bool
  | Uast.Tpreid q ->
     if (leaf q).pid_str = "empty" then
       let@ v = exist in
       let+ () = w --- S.ty_list v
       and+ v = decode w in
       mk_term Tnil (O.ty_list v)
     else
       let+ _ = instance q w
       and+ ty = decode w in
       mk_term (Tvar (X.to_string q)) ty
  | Uast.Tlet(id, t1, t2) ->
     let+ (_, (_, _), tt1, tt2)  = 
       let1 (Qpreid id) (hastype t1) (hastype t2 w) in
     mk_term (Tlet(id.pid_str, tt1, tt2)) tt2.ty
  | Uast.Tapply({term_desc=Uast.Tapply(c, t1); _}, t2) ->
     begin match c.term_desc with
     |Uast.Tpreid q ->
         let id = leaf q in
         if id.pid_str = "cons" then
           let@ v = exist in
           let+ () = w --- S.ty_list v
           and+ tt1 = hastype t1 v
           and+ tt2 = hastype t2 w 
           and+ t = decode w in
           mk_term (Tcons(tt1, tt2)) t
         else
           assert false
     | _ -> assert false end
  | _ -> assert false

let tbl : (string, O.ty) Hashtbl.t = Hashtbl.create 100

let rec print_ty = 
  let pp = Fmt.pf Format.std_formatter "%s" in
  function
  |O.Bool -> pp "bool\n"
  |O.Int -> pp "int\n"
  |List t -> pp "list "; print_ty t
  |Var x -> pp "'"; pp x

let typecheck t =
  let _, t = try Solver.solve ~rectypes:false (let0 (exist (hastype t))) with
  |Solver.Unbound (_, t) ->
    print_endline (X.to_string t);
      raise Exit in print_ty t.ty; t
