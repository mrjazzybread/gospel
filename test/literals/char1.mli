val f : char -> unit
(*@ requires c = '\'
    let _ = f c
*)

(* {gospel_expected|
   [125] File "char1.mli", line 2, characters 17-18:
         2 | (*@ requires c = '\'
                              ^
         Error: Illegal character '.
   |gospel_expected} *)
