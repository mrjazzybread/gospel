(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

module W = Warnings
open Ppxlib

let parse_ocaml_lb ~parse lb =
  let lb_pps = Fmt.str "%a" Pps.run lb |> Lexing.from_string in
  Location.init lb_pps lb.lex_start_p.pos_fname;
  try parse lb_pps
  with _ ->
    let loc_start, loc_end = (lb_pps.lex_start_p, lb_pps.lex_curr_p) in
    let loc = Location.{ loc_start; loc_end; loc_ghost = false } in
    W.error ~loc W.Syntax_error

let parse_ocaml ~parse file =
  let lb = open_in file |> Lexing.from_channel in
  Location.init lb file;
  parse_ocaml_lb ~parse lb

let parse_signature path =
  let ocaml = parse_ocaml ~parse:Parse.interface path in
  Uattr2spec.signature ~filename:path ocaml

let parse_implementation path =
  let ocaml = parse_ocaml ~parse:Parse.implementation path in
  Uattr2spec.implementation ~filename:path ocaml
