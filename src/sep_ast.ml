open Ppxlib
open Tterm
open Tast

type sep_term = 
  |Star of sep_term list
  |Pure of term
  |App of Symbols.lsymbol * Symbols.vsymbol list
  |Magic of sep_term * sep_term
  |Forall of Symbols.vsymbol list * sep_term 
  |Exists of Symbols.vsymbol list * sep_term 
  |Lambda of Symbols.vsymbol list * sep_term
  |RO of sep_term
  |Top

type type_definition = {
  t_name : Ident.t;
  t_args : Ident.t list;
}

type triple = {
  triple_name : Ident.t;
  triple_args : Symbols.vsymbol list;
  triple_pre : sep_term;
  triple_type : core_type;
  triple_post : sep_term;
}

type definition_node = 
  |Pred of Ident.t * Symbols.vsymbol list
  |Type of Ident.t * Ttypes.tvsymbol list
  |Triple of triple
  |Axiom of Tast.axiom
  |Function of Tast.function_

type definition = {
  d_node : definition_node;
  d_loc : Location.t;
}


let rev (_:'a list) : 'a list = assert false
	(*requires l <> nil
	ensures l <> nil
x:int
y:bool
(dyn x)::(dyn y)::nil : list dyn

dyn := { A : Type;  v : A }
*)

(*Parameter rev_spec : forall A (EA:Enc A) (l:list A),
  Triple (trm_Apps rev [dyn l])
	 PRE (hpure [l <> nil])
    POST (fun (res:list A) => (hpure [res <> nil]))
*)
(*hstar
x ~> R X  == hdata (R X) x
R X x

let def =

   let targs = [("l", coq_apps_var "list" [coq_var "A"])] in
   let dynargs = List.map (fun (x,t) -> coq_dyn_of t (coq_var x)) targs in
   (* maybe generating (dyn x) could be fine   coq_app_var "dyn" (coq_var x)  *)
    (*   (@dyn t _ x) where _ will triger the lookup of a [Enc t] instance *)
    (* Axiom Enc_any : forall A, Enc A *)
    (* Enc A := { enc : A -> val } *)
   let trm = trm_apps_lifted (coq_var "rev") dynargs in
   let pre = hpure (coq_app_neq (coq_var "l") (coq_app_var_at "nil" [coq_var "A"])) in
   let postbody = hpure (coq_app_neq (coq_var "res") (coq_app_var_at "nil" [coq_var "A"])) in
   let post = coq_fun ("l'", coq_apps_var "list" [coq_var "A"]) postbody in
   let triple = coq_apps_var "CFML.SepLifted.Triple" [trm; pre; post] in
   let stmt = coq_forall_enc_types ["A"] (coq_foralls targs triple) in
   coqtop_params [ ("rev_spec", stmt) ]
   let coqtop_require_unless flag modules =
    if not flag then
      [ Coqtop_require modules ]
    else
      []
  
  let require modules =
    if modules = [] then [] else [ Coqtop_require modules ]
    let cfml ss =
      List.map (fun s -> "CFML." ^ s) ss
    

let coqtops = 
      Coqtop_set_implicit_args ::
      Coqtop_require [ "Coq.ZArith.BinInt"; "TLC.LibLogic"; "TLC.LibRelation"; "TLC.LibInt"; "TLC.LibListZ" ] ::
      Coqtop_require (cfml ["SepBase"; "SepLifted"; "WPLifted"; "WPRecord"; "WPArray"; "WPBuiltin" ]) ::
      (*coqtop_require_unless no_mystd_include*)
      Coqtop_require (cfml [ "Stdlib.Array_ml"; "Stdlib.List_ml"; "Stdlib.Sys_ml" ]) ::
      Coqtop_require_import [ "Coq.ZArith.BinIntDef"; "CFML.Semantics"; "CFML.WPHeader" ] ::
      Coqtop_custom "Delimit Scope Z_scope with Z." ::
      [Coqtop_custom "Existing Instance WPHeader.Enc_any | 99."](* @
      require (filter no_mystd_include (external_modules())*)
    @ def

let result = Print_coq.tops coqtops
let _ = file_put_contents ("myfile.v") result

*)
