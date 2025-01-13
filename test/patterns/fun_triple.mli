val f : ('a * 'b * 'c) list -> 'a list
(*@ let xs = f ys in
    ensures xs = Sequence.map (fun (x, _, _) : 'a -> x) ys
*)
