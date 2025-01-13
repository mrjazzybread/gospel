val f : (int * int) option -> unit
(*@ modifies x
    requires match x with
             | Some (_,_) -> true
             | _ -> false
    let _ = f x
*)
