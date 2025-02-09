(*@ predicate (->) (p : bool) (q : bool) *)

(*@ predicate (/\) (p : bool) (q : bool) *)

(*@ predicate (\/) (p : bool) (q : bool) *)

(*@ predicate (<->) (p : bool) (q : bool) *)

(* Boolean operators *)

(*@ function (&&) (b1 : bool) (b2 : bool) : bool *)

(*@ function (||) (b1 : bool) (b2 : bool) : bool *)

(* {1 Arithmetic}

    The type [integer] is built-in. This is the type of arbitrary precision
    integers, not to be confused with OCaml's type [int] (machine, bounded
    integers). *)

(*@ function succ (x: integer) : integer *)
(*@ function pred (x: integer) : integer *)

(*@ function (-_) (x: integer) : integer *)
(*@ function (+) (x y: integer) : integer *)
(*@ function (-) (x y: integer) : integer *)
(*@ function ( * ) (x y: integer) : integer *)
(*@ function (/) (x y: integer) : integer *)
(*@ function mod (x y: integer) : integer *)

(*@ function pow (x y: integer) : integer *)
(*@ function abs (x:integer) : integer *)

(*@ function min (x y : integer) : integer *)
(*@ function max (x y : integer) : integer *)

(* {2 Comparisons} *)

(*@ predicate (>) (x y: integer) *)
(*@ predicate (>=) (x y: integer) *)
(*@ predicate (<) (x y: integer) *)
(*@ predicate (<=) (x y: integer) *)

(*@ function max_int : integer *)
(*@ function min_int : integer *)

(* {1 Sequences} *)

(*@ predicate monoid (f : integer -> integer -> integer) (neutral : integer) *)

(*@ predicate (==) (x : integer) (y : integer) *)

(*@ axiom monoid_def :
      forall f neutral.
      monoid f neutral <->
        (forall x. (f neutral x == f x neutral == x)) /\
        (forall x y z. f x (f y z) == f (f x y) z) *)

(*@ predicate comm_monoid (f : integer -> integer -> integer) (neutral : integer) *)

(*@ axiom comm_monoid_def :
      forall f neutral.
      comm_monoid f neutral <->
        monoid f neutral /\
        (forall x y. f x y == f y x) *)

module M : sig
  (*@ function f (x : integer) : integer *)
end

(*@ axiom epic : M.f 0 == M.f 0 *)
