val f : (int * 'a) list -> int list
(*@ let ys = f xs in
    ensures ys = Sequence.map (fun (x, _) -> x) xs
*)
