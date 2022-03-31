type _ effect = Div_by_zero : int effect

type exp = Const of int | Div of exp * exp

let eval e = match e with
|Const n -> n 
|Div (l, r) -> 
  let eval_l = eval l in
  let eval_r = eval r in 
    if eval_r = 0 
      then perform Div_by_zero 
      else eval_l / eval_r  

let main e =
  try_with (fun e -> eval e) e 
  {effc = fun e -> match e with |Div_by_zero x -> Some (fun k -> continue k 1000) |_ -> None} [@gospel {|try_ensures true|}]

let f = 
  let x = 0 in
  let y = 0 in 0