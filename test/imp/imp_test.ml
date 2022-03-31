

let f = try_with (fun x -> x) 1 {effc = fun e -> match e with |Effect1 x -> Some (fun k -> x) |Effect2 x -> Some (fun k -> x) |_ -> None} [@gospel {|try_ensures true|}]

let eval e = match e with
|Const n -> n 
|Div (l, r) -> 
  let eval_l = eval l in
  let eval_r = eval r in 
    eval_l / eval_r  
