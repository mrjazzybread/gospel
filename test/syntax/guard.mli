val f : int -> int
(*@ requires 0 = match x with _ when match 1 with _ -> true -> 1 | _ -> 2
    let y = f x
*)

(* missing parentheses around nested match, as the above expression is parsed
   as:
     match x with
       _ when (match 1 with _ -> (true -> 1)
                          | _ -> 2)
*)

(* {gospel_expected|
   [125] File "guard.mli", line 3, characters 4-7:
         3 |     let y = f x
                 ^^^
         Error: Syntax error.
   |gospel_expected} *)
