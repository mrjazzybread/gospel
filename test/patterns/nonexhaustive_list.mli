val f : 'a list -> int
(*@ requires match l with
      | [] -> true
      | x::y::_ -> false
    let r = f l
*)

(* {gospel_expected|
   [125] File "nonexhaustive_list.mli", line 2, characters 13-69:
         2 | .............match l with
         3 |       | [] -> true
         4 |       | x::y::_ -> false
         Error: This pattern-matching is not exhaustive.
                Here is an example of a case that is not matched:
                  infix :: (_, []).
   |gospel_expected} *)
