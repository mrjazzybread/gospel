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
open IdUast
module Env = Map.Make (String)
open Itypes
module W = Warnings

(** Since we use Inferno for type inference, we must take some pre processing
    steps to deal with potential problems within Gospel specifications that are
    not caught by it. These include:

    Modules: In Gospel (like in OCaml), the name we use to reference a variable
    depends on where we reference it. For example, if we define a function [f]
    in a module [M], within that module, that function's name is [f], but
    outside of that module it is [M.f]. If we [open] the module, both names are
    valid. Since Inferno does not support any of these mechanisms, we must take
    care to tag every variable with a unique identifier. In our previous
    example, regardless of what name we use to reference [f], each instance of
    that variable will be tagged with the same identifier. This means when we
    build our constraints, it won't matter that the name of the variable is
    context dependent, since Inferno will only look at the unique identifier. A
    consequence of this is that that there should be no unbound variables when
    we build the Inferno constraint, since to tag every variable we must keep
    track of what variables have been defined. This would be difficult to do in
    [Checker] since the Inferno constraint has to be built bottom-up, that is,
    starting from the last Gospel definition and this process is simpler when
    done top down.

    Infix operators: During parsing, we use the [Tinfix] constructor to mark a
    chain of infix operators. After parsing however, this constructor is
    redundant since during typechecking, it is equivalent to two nested [Tapply]
    constructors. Therefore, to simplify building the Inferno constraint, we
    remove this constructor from all terms and replace it with [Tapply].
    Although this step could be done in [Checker], it makes more sense to do it
    during this phase.

    Duplicate names: There are some contexts in Gospel where we are not allowed
    to introduce the same variable twice into scope (e.g. duplicate function
    arguments). Since Inferno has no way to track this natively, we must handle
    this manually. Although this could be done in [Checker], it would lead to a
    strange situation since the Inferno constraint is built bottom up, these
    errors would also appear from the bottom up. However, normal typechecking
    errors would appear top down. This would lead to very non-ergonomic
    specification writing since Gospel errors would pop up in a completely
    unexpected order. *)

type namespace = UniqueId.t Env.t
(** Maps variable names to their unique identifiers *)

type mod_defs = {
  fun_env : namespace; (* Function definitions *)
  type_env : namespace; (* Type definitions *)
  mod_env : namespace; (* Nested modules *)
  nested_defs : mod_defs Env.t;
      (* Nested modules definitions. Has the same domain has [mod_env]. *)
}
(** Set of top level module definitions *)

(** Represents all the definitions within a module *)
let empty_defs =
  {
    fun_env = Env.empty;
    type_env = Env.empty;
    mod_env = Env.empty;
    nested_defs = Env.empty;
  }

(** [defs_union defs scope] creates a new environment with all of the
    definitions in [defs] and [scope]. In case there are definitions with the
    same name, precendence is given to those in [defs]. *)
let defs_union defs scope =
  let f _ x _ = Some x in
  let u = Env.union f in
  {
    fun_env = u defs.fun_env scope.fun_env;
    type_env = u defs.type_env scope.type_env;
    mod_env = u defs.mod_env scope.mod_env;
    nested_defs = Env.union f defs.nested_defs scope.nested_defs;
  }

(** [lookup f defs pid] accesses the namespace [f defs] and returns the data
    associated with [pid]. If [f] is a field access to [netsed_defs], returns
    the definitions associated with the module [pid]. Otherwise returns the
    unique identifier associated with [pid].
    @raise Not_found
      when there is no identifer with name [pid.pid_str] in the namespace
      associated with [f defs] *)
let lookup f (defs : mod_defs) pid =
  let env = f defs in
  let str = pid.Preid.pid_str in
  Env.find str env

(* Helper functions for field accesses *)
let find_fun = fun d -> d.fun_env
let find_type = fun d -> d.type_env
let find_mod = fun d -> d.mod_env
let find_defs = fun d -> d.nested_defs

let add_fun f defs =
  let env = defs.fun_env in
  { defs with fun_env = Env.add f.UniqueId.id_str f env }

let add_type ty defs =
  let env = defs.type_env in
  { defs with type_env = Env.add ty.UniqueId.id_str ty env }

let add_mod m mod_defs defs =
  let menv = defs.mod_env in
  let denv = defs.nested_defs in
  {
    defs with
    mod_env = Env.add m.UniqueId.id_str m menv;
    nested_defs = Env.add m.UniqueId.id_str mod_defs denv;
  }

(** [access_mod defs q] returns a qualified identifer where the identifiers used
    in [q] have been replaced with uniquely tagged identifiers. This function
    assumes that all the identifiers in [q] are belong to the module namespace.
    This function also returns the definitions contained in the module
    associated with [q].
    @raise Not_found
      if any of the identifiers in [q] are not defined. TODO, This exception
      should be caught here and subsequently throw a Gospel exception. *)
let rec access_mod defs = function
  | ParseUast.Qid pid ->
      let id = lookup find_mod defs pid in
      let defs = lookup find_defs defs pid in
      (Qid id, defs)
  | Qdot (q, pid) ->
      let q, defs = access_mod defs q in
      let id = lookup find_mod defs pid in
      let defs = lookup find_defs defs pid in
      (Qdot (q, id), defs)

(** [qualid_to_unique f defs q] returns a qualified identifer where the
    identifiers used in [q] have been replaced with uniquely tagged identifiers.
    The [defs] parameter is the environment we will use the search for the
    tagged identifiers and [f] is the namespace we will search the outermost
    identifier of [q]. *)
let unique_toplevel_qualid f defs = function
  | ParseUast.Qid pid ->
      (* If there are no module accesses, we lookup [id] in the
       environment [f defs]. *)
      Qid (lookup f defs pid)
  | Qdot (q, pid) ->
      (* If there are module accesses, we first get the environment
       associated with the module represented by [q], and then lookup
       [id] in that environment. *)
      let q, defs = access_mod defs q in
      Qdot (q, lookup f defs pid)

type local_env = {
  term_var : UniqueId.t Env.t;
  (* Local term variables bound with a [let] or a quantifier *)
  type_var : UniqueId.t Env.t; (* Type variables. *)
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
  { env with type_var = Env.add var id env.term_var }

(** [unique_pty defs env pty] Returns a type annotation where each type symbol
    (including type variables) has been replaced with a unique identifier. Type
    variables are searched in [env] and type names are search in [defs]. Type
    annotations are allowed to introduce type variables into the local scope but
    are not allowed to introduce fresh type names. When new type variables are
    introduced, the [env] is modified in place. *)
let unique_pty defs env =
  let rec unique_pty = function
    | ParseUast.PTtyvar id when Env.mem id.pid_str !env.type_var ->
        PTtyvar (Env.find id.pid_str !env.type_var)
    | PTtyvar pid ->
        let id = UniqueId.from_preid pid in
        env := add_type_var pid.pid_str id !env;
        PTtyvar id
    | PTtyapp (q, l) ->
        let q = unique_toplevel_qualid find_type defs q in
        PTtyapp (q, List.map unique_pty l)
    | PTtuple l -> PTtuple (List.map unique_pty l)
    | PTarrow (label, arg, res) ->
        let label = unique_label label in
        let arg = unique_pty arg in
        let res = unique_pty res in
        PTarrow (label, arg, res)
  and unique_label = function
    | ParseUast.Lnone id when id.pid_str = "_" -> Lnone (UniqueId.from_preid id)
    | _ -> assert false
  in
  unique_pty

(** [unique_var env defs q] returns a qualified identifer where the identifiers
    used in [q] have been replaced with uniquely tagged identifiers. If [q] has
    no module accesses (i.e. is not of the form id1.id2) then this function
    returns the binding in [env] if it exists. Otherwise it uses [defs] to
    search for the tagged identifiers. *)
let unique_var env defs q =
  match q with
  | ParseUast.Qid pid when Env.mem pid.pid_str env ->
      IdUast.Qid (Env.find pid.pid_str env)
  | _ ->
      (* If [q] is of the form [Qdot] it cannot be a local variable*)
      unique_toplevel_qualid find_fun defs q

(** [unique_term defs env t] returns a term where every variable in [t] has been
    replaced with a uniquely tagged variable. When we find a free variable, we
    first search the local scope. If it is not found then we search the top
    level. When a variable is bound either with a [let] or a quantifier, we
    create a new unique identifier and map it to [env]. *)
let rec unique_term defs env t =
  (* The namespace remains constant in each recursive call *)
  let unique_term = unique_term defs in
  let term_desc =
    match t.ParseUast.term_desc with
    | ParseUast.Ttrue -> Ttrue
    | Tfalse -> Tfalse
    | Tconst c -> Tconst c
    | Tvar q -> Tvar (unique_var env.term_var defs q)
    | Tlet (v, t1, t2) ->
        let id = UniqueId.from_preid v in
        let t1 = unique_term env t1 in
        (* Add the identifier to the local environment*)
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
          let id = UniqueId.from_preid pid in
          env := add_term_var pid.pid_str id !env;
          let pty = Option.map (unique_pty defs env) pty in
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
    | _ -> assert false
  in
  { term_desc; term_loc = t.term_loc }

(* Helper functions for top level signatures *)

let function_ f defs =
  let fun_name = UniqueId.from_preid f.ParseUast.fun_name in
  let fun_rec = f.fun_rec in
  let env = ref empty_local_env in
  let fun_type = Option.map (unique_pty defs env) f.fun_type in
  let fun_params =
    List.map
      (fun (loc, pid, pty) ->
        let id = UniqueId.from_preid pid in
        (* Add the function parameters into the local environment. *)
        env := add_term_var pid.pid_str id !env;
        (* Add the type variables in [pty] to the local environment. *)
        let pty = unique_pty defs env pty in
        (loc, id, pty))
      f.fun_params
  in
  let fun_def = Option.map (unique_term defs !env) f.fun_def in
  let fun_spec = Option.map (fun _ -> assert false) f.fun_spec in
  let fun_loc = f.fun_loc in
  let fun_text = f.fun_text in
  {
    fun_name;
    fun_rec;
    fun_type;
    fun_params;
    fun_def;
    fun_spec;
    fun_loc;
    fun_text;
  }

let axiom defs ax =
  let ax_name = UniqueId.from_preid ax.ParseUast.ax_name in
  let ax_term = unique_term defs empty_local_env ax.ax_term in
  let ax_loc = ax.ax_loc in
  let ax_text = ax.ax_text in
  { ax_name; ax_term; ax_loc; ax_text }

type env = {
  defs : mod_defs;
  (* Contains the top level definitions in the current module *)
  scope : mod_defs;
      (* Contains the top level definitions currently in scope. A definition is in
     scope if it has been previously defined or exposed through an [open]. Not
     necessarily a subset of [defs_mod], since opening modules can shadow
     previous definitions. *)
}
(** Environment for processing top level signatures. *)

(** [signature s] processes [f] and changes the global variables [defs_mod] and
    [defs_scope] whenever a new name is added to the top level. *)
let rec signature s env =
  let add_def f env = { defs = f env.defs; scope = f env.scope } in
  let sdesc, env =
    match s.ParseUast.sdesc with
    | ParseUast.Sig_function f ->
        let f = function_ f env.scope in
        (* Adds [f] to the set of variables defined and to the
          scope *)
        let env = add_def (add_fun f.fun_name) env in
        (Sig_function f, env)
    | Sig_axiom ax ->
        (* Since axioms cannot be referenced, the environment is not
          modified.*)
        let ax = axiom env.scope ax in
        (Sig_axiom ax, env)
    | Sig_module m ->
        (* TODO figure out when this is None *)
        let id = Option.map UniqueId.from_preid m.mdname in
        (* Since we are now at the beginning of a new module, we
          create an environment with the same variables in scope,
          but with no module definitions. *)
        let sub_mod_env = { env with defs = empty_defs } in
        let mdesc, mod_defs =
          match m.mdtype.mdesc with
          | Mod_signature m ->
              (* The returned environment contains the all the
                definitions in this submodule. *)
              let s, env = signatures m sub_mod_env in
              (* We ignore the scope as it is not relevant after
                processing the module. *)
              (Mod_signature s, env.scope)
          | _ -> assert false
        in
        (* If this module has an identifier, then this function adds
           it to the current definitions *)
        let f defs =
          Option.fold ~none:defs ~some:(fun id -> add_mod id mod_defs defs) id
        in
        (* Rebuild the module object *)
        let env = add_def f env in
        let mloc = m.mdtype.mloc in
        let mattributes = m.mdtype.mattributes in
        let mdtype = { mdesc; mloc; mattributes } in
        let s =
          Sig_module
            {
              mdname = id;
              mdtype;
              mdattributes = m.mdattributes;
              mdloc = m.mdloc;
            }
        in
        (s, env)
    | _ -> assert false
  in
  ({ sdesc; sloc = s.sloc }, env)

(** [signatures l env] Processes a list of top level signatures along with the
    current environment. *)
and signatures (l : ParseUast.s_signature) env =
  match l with
  | [] -> ([], env)
  | x :: t ->
      let s, env = signature x env in
      let t, env = signatures t env in
      (s :: t, env)

(** An environment with primitive type definitions. *)
let type_env =
  List.fold_left (fun env (x, y) -> Env.add x y env) Env.empty S.primitive_list

(** The empty environment *)
let empty_env = { defs = empty_defs; scope = { empty_defs with type_env } }

let signatures l = fst (signatures l empty_env)
