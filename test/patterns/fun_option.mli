val f : 'a option list -> bool
(*@ let b = f os in
    ensures Sequence._exists (fun (None | Some _) -> false) os
*)
