val hd : 'a list -> 'a
(*@ match hd l with
    |exception Failure -> ensures l = [] *)

val find : ('a -> bool) -> 'a list -> 'a
(*@ match find f l with
    |exception Not_found -> 
      ensures forall x. Sequence.mem x l -> not f x *)

val invalid_arg : string -> 'a
(*@ match invalid_arg s with
    |exception Invalid_argument
    |_ -> ensures false *)
