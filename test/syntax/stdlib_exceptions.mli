val hd : 'a list -> 'a
(*@ let x = hd l in
      raises Failure _ -> l = [] *)

val find : ('a -> bool) -> 'a list -> 'a
(*@ let r = find f l in
      raises Not_found -> forall x. Sequence.mem x l -> not f x *)

val invalid_arg : string -> 'a
(*@ raises  Invalid_argument _ -> true
    ensures false *)
