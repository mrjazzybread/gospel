(*@ protocol exp : ensures true *)

(*@ protocol exp : requires true *)

(*@ protocol exp : modifies x *)

(*@ protocol exp : reply_type unit *)
let x  = try_with f arg {effc = (fun e -> match e with |Effect1 -> Some (fun k -> wow) |_ -> None)}  [@gospel {|try_ensures wow|}]