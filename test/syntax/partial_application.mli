val scalar_product : int list -> int -> int list
(*@ let r = scalar_product v s in
    ensures Sequence.map integer_of_int r = Sequence.map ((fun x y -> x * integer_of_int y) (integer_of_int s)) v *)
