(*@ function fibonacci (n: integer) : integer *)

val fib : int -> int -> int -> int
(*@ requires i >= 0
    checks n >= 0
    requires a = fibonacci i
    requires b = fibonacci (i+1)
    let r = fib [i: integer] n a b in
    ensures r = fibonacci (i+n) *)
