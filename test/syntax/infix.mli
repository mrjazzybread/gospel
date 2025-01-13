val ( == ) : 'a -> 'a -> bool
(*@ let r = (==) x y in
      ensures r <-> x = y *)

val ( == ) : 'a -> 'a -> bool
(*@ let r = x == y in
      ensures r <-> x = y *)
