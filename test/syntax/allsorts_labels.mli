(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

val f : int -> int -> int
(*@ let r = f y x in  
      ensures true *)

val f : y:int -> int -> int
(*@ let r = f ~y x in  
      ensures true *)

val f : ?y:int -> int -> int
(*@ let r = f ?y x in  
      ensures true *)

val f : y:int -> ?x:int -> int
(*@ let r = f ~y ?x in  
      ensures true *)

val f : ?y:int -> x:int -> int
(*@ let r = f ?y ~x in  
      ensures true *)

val f : ('a -> 'b -> 'c) -> 'a -> 'b -> 'c
(*@ let r = f x y z in  
      ensures true *)

val f : x:('a -> 'b -> 'c) -> 'a -> 'b -> 'c
(*@ let r = f ~x y z in  
      ensures true *)

val f : x:('a -> 'b -> 'c) -> y:'a -> 'b -> 'c
(*@ let r = f ~x ~y z in  
      ensures true *)

val f : x:('a -> 'b -> 'c) -> y:'a -> 'b -> 'c
(*@ let r = f ~x [w:int] ~y z in  
      ensures true *)

val f : x:('a -> 'b -> 'c) -> y:'a -> 'b -> 'c
(*@ let r = f ~x [w:int] ~y [p:integer] z in  
      ensures true *)

val f : x:('a -> 'b -> 'c) -> y:'a -> 'b -> 'c
(*@ let r,[a:'a] = f ~x [w:int] ~y [p:integer] z in  
      ensures true *)

val f : x:('a -> 'b -> 'c) -> y:'a -> 'b -> 'c
(*@ let [b:integer],r,[a:'a] = f ~x [w:int] ~y [p:integer] z in  
      ensures true *)

val f : x:('a -> 'b -> 'c) -> y:'a -> 'b -> 'c
(*@ let [b:integer],r,[a:'a] = f ~x [w:int] ~y [p:integer] z in  
      ensures true *)

val f : int ref -> unit
(*@ modifies x
    let () = f x *)
