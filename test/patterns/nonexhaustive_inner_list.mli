val f : 'a list option -> int
(*@ requires match l with
      | None -> false
      | Some (x :: _ as a) -> false
    let r = f l
*)

(* {gospel_expected|
   [125] File "nonexhaustive_inner_list.mli", line 2, characters 13-83:
         2 | .............match l with
         3 |       | None -> false
         4 |       | Some (x :: _ as a) -> false
         Error: This pattern-matching is not exhaustive.
                Here is an example of a case that is not matched:
                  Some [].
   |gospel_expected} *)
