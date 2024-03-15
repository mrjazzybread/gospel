open Ppxlib
open Identifier
open Tterm
open Ttypes
open Symbols

type dty = Tvar of dtvar | Tapp of tysymbol * dty list | Tty of ty
and dtvar = { dtv_id : int; mutable dtv_def : dty option }

module Mstr : Map.S with type key = string

type dpattern = {
  dp_node : dpattern_node;
  dp_dty : dty;
  dp_vars : dty Mstr.t;
  dp_loc : Location.t;
}

and dpattern_node =
  | DPwild
  | DPvar of Preid.t
  | DPapp of lsymbol * dpattern list
  | DPor of dpattern * dpattern
  | DPas of dpattern * Preid.t
  | DPcast of dpattern * dty
  | DPconst of Parsetree.constant
  | DPinterval of char * char

type dbinder = Preid.t * dty

type dterm = { dt_node : dterm_node; dt_dty : dty; dt_loc : Location.t }

and dterm_node =
  | DTattr of dterm * string list
  | DTvar of Preid.t
  | DTconst of constant
  | DTapp of lsymbol * dterm list
  | DTif of dterm * dterm * dterm
  | DTlet of Preid.t * dterm * dterm
  | DTcase of dterm * (dpattern * dterm option * dterm) list
  | DTquant of quant * dbinder list * dterm
  | DTlambda of dpattern list * dterm
  | DTbinop of binop * dterm * dterm
  | DTnot of dterm
  | DTold of dterm

type 'a full_env = {env : 'a Mstr.t; old_env : 'a Mstr.t}

type denv = dty full_env
type env = vsymbol full_env

val env_union :
  (label -> 'a -> 'a -> 'a option) ->
  'a full_env ->
  'a Mstr.t  ->
  'a full_env

val dty_bool : dty
val dty_char : dty
val dty_float : dty
val dty_string : dty
val dty_integer : dty
val dty_int : dty
val dty_of_ty : Ttypes.ty -> dty
val dty_fresh : unit -> dty
val specialize_ls : lsymbol -> dty list * dty
val specialize_cs : loc:Location.t -> lsymbol -> dty list * dty
val dty_unify : loc:Location.t -> dty -> dty -> unit
val dterm_unify : dterm -> dty -> unit
val dfmla_unify : dterm -> unit
val max_dty : Coercion.t -> dterm list -> dty

val app_unify :
  loc:Location.t -> lsymbol -> ('a -> 'b -> unit) -> 'a list -> 'b list -> unit

val app_unify_map :
  loc:Location.t -> lsymbol -> ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list

val dpattern_unify : dpattern -> dty -> unit
val unify : dterm -> dty -> unit
val dterm_expected : Coercion.t -> dterm -> dty -> dterm
val dfmla_expected : Coercion.t -> dterm -> dterm
val denv_get_opt : 'a full_env -> string -> 'a option
val denv_find : loc:Location.t -> string -> denv -> dty
val is_in_denv : denv -> string -> bool
val denv_add_var : denv -> string -> dty -> denv
val denv_add_var_quant : denv -> (Identifier.Preid.t * dty) list -> denv
val term : env -> dterm -> term
val pattern : dpattern -> Tterm.pattern * vsymbol Mstr.t
