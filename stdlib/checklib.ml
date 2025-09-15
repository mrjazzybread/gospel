(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

open Gospel_checker

module W = Warnings
(** Simple script to type check the Gospel and OCaml standard library and
    generate a [.gospel] file for it. *)

let () =
  let _ =
    let () = Ident.Tag.set_project_name Ident.stdlib_project in
    let _, defs =
      Bin_utils.check_file ~comp:true ~verbose:false "gospelstdlib.mli"
    in
    let env = Namespace.init_env defs in
    let t, _ =
      Bin_utils.check_file ~comp:true ~verbose:false ~env "ocamlprimitives.mli"
    in
    let ns = Sep_utils.empty_env () in
    let defs = Semantics.process_sigs ns t in
    let out = open_out "ocamlprimitives.sep" in
    Marshal.to_channel out ns [];
    close_out out;
    defs
  in
  ()
