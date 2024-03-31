(*@ predicate ho (f : integer -> prop) (n : integer) = f n *)

(*@ predicate fo (n : integer) *)

(*@ function fof (n : integer) : bool *)

(*@ predicate test (n : integer) = ho fo n  && ho fof n *)

val neg : bool -> bool
(*@ r = neg b
    ensures b <-> not r *)

(*@ r = neg b
    ensures
      if b then True else False <-> not (if r then True else False) *)
