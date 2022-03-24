[@@@gospel {| protocol exp : ensures true|}]

[@@@gospel {| protocol exp : requires false|}]

[@@@gospel {| protocol exp : modifies x|}]

[@@@gospel {| protocol exp : reply_type unit|}]

let x = try_with 0 1

(*

let () =
    try_with (fun x -> x) () {effc = fun e -> E }


    match let x = () in x with
    |Result r -> r
    |Effect e -> E*)