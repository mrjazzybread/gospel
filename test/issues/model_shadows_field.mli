type 'a t = { contents : 'a list }
(*@ mutable model contents: 'a list *)

val f : 'a t -> unit
(*@ modifies xs
    let _ = f xs in
      ensures xs = { contents = [] }
*)

(* {gospel_expected|
   [125] File "model_shadows_field.mli", line 7, characters 19-36:
         7 |       ensures xs = { contents = [] }
                                ^^^^^^^^^^^^^^^^^
         Error: The record field contents does not exist.
   |gospel_expected} *)
