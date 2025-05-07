(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

module Env = Map.Make (String)
open Uast
open IdUast
module W = Warnings
open Namespace

type local_env = {
  term_var : Ident.t Env.t;
  (* Local term variables bound with a [let] or a quantifier *)
  type_var : Ident.t Env.t; (* Type variables. *)
}
(** Keeps track of the variables within the scope of a term. *)

let empty_local_env = { term_var = Env.empty; type_var = Env.empty }

(** [add_term_var var id env] maps the term variable [var] to the tagged
    identifier [id] *)
let add_term_var var id env =
  { env with term_var = Env.add var id env.term_var }

(** [add_type_var var id env] maps the type variable [var] to the tagged
    identifier [id] *)
let add_type_var var id env =
  { env with type_var = Env.add var id env.type_var }

(** [unique_pty ~bind defs env pty] Returns a type annotation where each type
    symbol (including type variables) has been replaced with a unique
    identifier. Type variables are searched in [env] and type names are search
    in [defs]. Type annotations are allowed to introduce type variables into the
    local scope if the [bind] flag is [true] but are never allowed to introduce
    fresh type names. When we encounter a type variable that is not in [env], we
    either add it or raise an exception depending on the value of [bind]. *)
let unique_pty ~bind defs env =
  let rec unique_pty = function
    | ParseUast.PTtyvar pid when Env.mem pid.pid_str !env.type_var ->
        PTtyvar (Env.find pid.pid_str !env.type_var)
    | PTtyvar pid when bind ->
        (* If we are in a context where we can introduce fresh type variables
           and [pid] is not in [env], we create a new identifier and add it. *)
        let id = Ident.from_preid pid in
        env := add_type_var pid.pid_str id !env;
        PTtyvar id
    | PTtyvar pid ->
        (* This case is only reached when the variable [pid] is unbound and we
          cannot add new type variables to the environment. *)
        W.error ~loc:pid.pid_loc (W.Unbound_type_variable pid.pid_str)
    | PTtyapp (q, l) ->
        (* When we encounter a type identifier, we must check if it is an alias
           for another type and if so replace it. *)
        resolve_alias defs q (List.map unique_pty l)
    | PTarrow (arg, res) ->
        let arg = unique_pty arg in
        let res = unique_pty res in
        PTarrow (arg, res)
    | PTtuple l -> PTtuple (List.map unique_pty l)
  in
  unique_pty

(* Helper function to facilitate using [unique_pty] *)

let unique_pty_bind = unique_pty ~bind:true

(* Although [unique_pty] expects a reference to a local scope, if [bind] is
   false, it is never modified since no type variables can be introduced. *)
let unique_pty defs env pty = unique_pty ~bind:false defs (ref env) pty

(** [unique_var env defs q] returns a qualified identifer where the identifiers
    used in [q] have been replaced with uniquely tagged identifiers. If [q] has
    no module accesses (i.e. is not of the form id1.id2) then this function
    returns the binding in [env] if it exists. Otherwise it uses [defs] to
    search for the tagged identifiers. *)
let unique_var env defs q =
  match q with
  | ParseUast.Qid pid when Env.mem pid.pid_str env ->
      Tlocal (Env.find pid.pid_str env)
  | _ ->
      (* If [q] is of the form [Qdot] it cannot be a local variable *)
      let q, params, id = fun_qualid defs q in
      Tvar (q, params, id)

(** [unique_term top defs env t] returns a term where every variable in [t] has
    been replaced with a uniquely tagged variable. When we find a free variable,
    we first search the local scope. If it is not found then we search the top
    level. When a variable from the top level is used, we must add its typing
    information to the AST. This is necessary for the type inference phase so
    that the Inferno solver knows the type of all the variables outside the
    scope of the term. When a variable is bound either with a [let] or a
    quantifier, we create a new unique identifier and map it in [env]. *)
let rec unique_term defs env t =
  (* The namespace remains constant in each recursive call *)
  let unique_term = unique_term defs in
  let term_desc =
    match t.ParseUast.term_desc with
    | ParseUast.Ttrue -> Ttrue
    | Tfalse -> Tfalse
    | Tconst c -> Tconst c
    | Tvar q -> unique_var env.term_var defs q
    | Tlet (v, t1, t2) ->
        let id = Ident.from_preid v in
        let t1 = unique_term env t1 in
        (* Add the identifier to the local environment *)
        let t2 = unique_term (add_term_var v.pid_str id env) t2 in
        Tlet (id, t1, t2)
    | Tapply (t1, t2) -> Tapply (unique_term env t1, unique_term env t2)
    | Tinfix _ -> (unique_term env (Uast_utils.chain t)).term_desc
    | Tquant (q, l, t) ->
        let env = ref env in
        (* [binder (pid, pty)] maps the string [pid.pid_str] to a
           fresh tagged variable, adds all the type variables present
           in [pty] to the local scope and turns [pty] into a tagged
           annotation. *)
        let binder (pid, pty) =
          let id = Ident.from_preid pid in
          env := add_term_var pid.pid_str id !env;
          let pty = Option.map (unique_pty_bind defs env) pty in
          (id, pty)
        in
        (* epic code *)
        let q = match q with Tforall -> Tforall | Texists -> Texists in
        let l = List.map binder l in
        let t = unique_term !env t in
        Tquant (q, l, t)
    | Tif (g, then_b, else_b) ->
        let g = unique_term env g in
        let then_b = unique_term env then_b in
        let else_b = unique_term env else_b in
        Tif (g, then_b, else_b)
    | Ttuple l -> Ttuple (List.map (unique_term env) l)
    | _ -> assert false
  in
  { term_desc; term_loc = t.term_loc }

(* Helper functions for top level signatures *)

(** [param_dups l] checks if there are any duplicate function parameters in [l].
    If so, this function throws an appropriate Gospel exception. If not, this
    function has no effect *)
let param_dups l =
  let open Preid in
  let error (x, _) = W.error ~loc:x.pid_loc (W.Duplicated_argument x.pid_str) in
  Utils.duplicate (fun (x, _) (y, _) -> eq x y) error l

let function_ f defs =
  let fun_name = Ident.from_preid f.ParseUast.fun_name in
  let fun_rec = f.fun_rec in
  let env = ref empty_local_env in
  let () = param_dups f.fun_params in
  let fun_type = Option.map (unique_pty_bind defs env) f.fun_type in
  let fun_params =
    List.map
      (fun (pid, pty) ->
        let id = Ident.from_preid pid in
        (* Add the function parameters to the local environment. *)
        env := add_term_var pid.pid_str id !env;
        (* Add the type variables in [pty] to the local environment. *)
        let pty = unique_pty_bind defs env pty in
        (id, pty))
      f.fun_params
  in
  let () =
    if fun_rec then
      (* If the function is recursive, add it as a local variable. *)
      env := add_term_var fun_name.id_str fun_name !env
    else ()
  in
  let fun_def = Option.map (unique_term defs !env) f.fun_def in
  let fun_spec =
    ignore f.fun_spec;
    None
  in
  let fun_loc = f.fun_loc in
  let f =
    { fun_name; fun_rec; fun_type; fun_params; fun_def; fun_spec; fun_loc }
  in
  f

let axiom defs ax =
  let ax_name = Ident.from_preid ax.ParseUast.ax_name in
  let ax_term = unique_term defs empty_local_env ax.ax_term in
  let ax_loc = ax.ax_loc in
  let ax_text = ax.ax_text in
  { ax_name; ax_term; ax_loc; ax_text }

let type_kind = function ParseUast.PTtype_abstract -> PTtype_abstract

let ghost_type_decl defs t =
  let tname = Ident.from_preid t.ParseUast.tname in
  let () =
    (* Raise an error if two type parameters have the same name *)
    let error x =
      W.error ~loc:x.Preid.pid_loc (W.Duplicated_parameter x.Preid.pid_str)
    in
    Utils.duplicate Preid.eq error t.tparams
  in
  let tparams = List.map Ident.from_preid t.tparams in
  (* Create an environment with the type variables in *)
  let env =
    List.fold_left
      (fun acc param -> add_type_var param.Ident.id_str param acc)
      empty_local_env tparams
  in
  (* We call [unique_pty] with the [bind] flag to false so that an error is
     raised if any type variables besides those defined in [tparams] are used. *)
  let tmanifest = Option.map (unique_pty defs env) t.tmanifest in
  let tprivate =
    match t.tprivate with Private -> Private | Public -> Public
  in
  {
    tname;
    tparams;
    tprivate;
    tkind = type_kind t.tkind;
    tmanifest;
    tattributes = t.tattributes;
    tspec = None;
    tloc = t.tloc;
  }

let rec process_module env m =
  (* TODO figure out when this is None *)
  let id = Option.map Ident.from_preid m.ParseUast.mdname in
  (* Since we are now at the beginning of a new module, we create an environment
     with the same variables in scope, but with no module definitions. *)
  let sub_mod_env = submodule env in
  let mdesc, mod_defs =
    match m.mdtype.mdesc with
    | Mod_signature m ->
        (* The returned environment contains the all the definitions in this
           submodule. *)
        let s, env = signatures m sub_mod_env in
        (* We ignore the [scope] field in [env] as it is not relevant after
           processing the module. *)
        (Tast2.Mod_signature s, defs env)
    | _ -> assert false
  in
  (* If this module has an identifier, then this function adds it to the current
     definitions *)
  let env =
    Option.fold ~none:env ~some:(fun id -> add_mod env id mod_defs) id
  in
  (* Rebuild the module object *)
  let mloc = m.mdtype.mloc in
  let mattributes = m.mdtype.mattributes in
  let mdtype = { Tast2.mdesc; mloc; mattributes } in
  let s =
    Tast2.Sig_module
      { mdname = id; mdtype; mdattributes = m.mdattributes; mdloc = m.mdloc }
  in
  (s, env)

(** [signature s env] processes [s] and returns a new environment where the
    names in [s] have been added to [env]. *)
and signature s env =
  let sdesc, env =
    match s.ParseUast.sdesc with
    | ParseUast.Sig_gospel (s, _) -> gospel_sig env s
    | Sig_module m -> process_module env m
    | Sig_attribute att -> (Sig_attribute att, env)
    | _ -> assert false
  in
  ({ Tast2.sdesc; sloc = s.sloc }, env)

and gospel_sig env = function
  | ParseUast.Sig_function f ->
      let f = function_ f (scope env) in
      let f = Solver.function_ f in
      let fun_ty = Tast2.fun_to_arrow f.fun_params f.fun_ret in
      let env = add_fun env f.fun_name fun_ty in
      (Tast2.Sig_function f, env)
  | Sig_axiom ax ->
      (* Since axioms cannot be referenced, the environment is not
        modified.*)
      let ax = axiom (scope env) ax in
      let ax = Solver.axiom ax in
      (Sig_axiom ax, env)
  | Sig_ghost_type t ->
      let t = ghost_type_decl (scope env) t in
      let alias = t.tmanifest in
      let env = add_type env t.tname t.tparams alias in
      (Sig_ghost_type t, env)
  | _ -> assert false

(** [signatures l env] Processes a list of top level signatures along with the
    current environment. *)
and signatures l env =
  match l with
  | [] -> ([], env)
  | s :: t ->
      let s, env = signature s env in
      let t, env = signatures t env in
      (s :: t, env)

let signatures l =
  let l, env = signatures l Namespace.empty_env in
  (l, Namespace.defs env)
