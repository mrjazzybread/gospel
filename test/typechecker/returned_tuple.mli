val f : unit -> int * int
(*@ let pair = f () in
    ensures true *)

(* {gospel_expected|
   [125] File "returned_tuple.mli", line 2, characters 15-16:
         2 | (*@ let pair = f () in
                            ^
         Error: Type checking error: too few returned values: when a function returns
                a tuple, the gospel header should name each member of the tuple; so
                the header of a function returning a pair might be "x,y = ...".
   |gospel_expected} *)
