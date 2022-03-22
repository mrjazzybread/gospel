
let add x y = x + y
(*@ ensures result = x + y
    requires x = y*)

let something x = Some x 
(*@ 
ensures true
protocol SOMETHING *)