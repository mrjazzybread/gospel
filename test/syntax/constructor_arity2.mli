type t = C of int * int

val f : int -> t -> unit
(*@ requires match t with
             | C (_,_,_)
             | _ -> true
    let () = f n t *)

(* {gospel_expected|
   [125] File "constructor_arity2.mli", line 5, characters 15-24:
         5 |              | C (_,_,_)
                            ^^^^^^^^^
         Error: The constructor C expects 2 argument(s)
                but is applied to 3 argument(s) here.
   |gospel_expected} *)
