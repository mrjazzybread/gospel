val f : int option -> int
(*@  requires match x with
             | Some (y:int) -> y >= 0
             | None -> true
     let r = f x *)
