type t

type 'a x = t

val f : int x
(*@ x = f
  ensures x = x *)
