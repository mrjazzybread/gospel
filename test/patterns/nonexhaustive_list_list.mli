val f : 'a list list -> int
(*@ requires match l with
      | [] -> true
      | []::_ -> false
      | (x::y::_)::_ -> false
    let r = f l
*)

(* {gospel_expected|
   [125] File "nonexhaustive_list_list.mli", line 2, characters 13-97:
         2 | .............match l with
         3 |       | [] -> true
         4 |       | []::_ -> false
         5 |       | (x::y::_)::_ -> false
         Error: This pattern-matching is not exhaustive.
                Here is an example of a case that is not matched:
                  infix :: (infix :: (_, []), []).
   |gospel_expected} *)
