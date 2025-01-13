type t = O | S of t

val succ : t -> t
(*@ let y = succ x in
    ensures y = S x *)

val test1 : t -> t -> t
(*@ requires x <> O && y = O
    let r = test1 x y in
    ensures  match x, y with
             | _, S _ -> false
             | O, _   -> false
             | S a, O -> r = S (S a) *)

(* pattern of type unit *)
val f_unit : int array -> unit
(*@ modifies a
    let x1 = f_unit a in
      ensures match x1 with () -> true *)

(*@ function fun_unit (x: unit): string =
    match x with
    | () -> "out" *)

type t2 = B of int * int

(*@ function f (x: t2) : unit =
    match x with B _ -> () *)
