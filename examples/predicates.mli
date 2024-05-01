(*@ predicate ho (f : integer -> bool) (n : integer)*)
(*@ predicate fo (n : integer) *)
(*@ predicate test (n : integer) = ho fo n *)
(*@ predicate eq (b1 : bool) (b2 : bool) = b1 = b2 *)
(*@ predicate eq_strange (b1 : bool) (b2 : bool) =
   (if test 0 then (=) else eq) b1 b2 *)

(*@ function flip (b : bool) : bool =
  match b with
  |true -> false
  |false -> true *)
