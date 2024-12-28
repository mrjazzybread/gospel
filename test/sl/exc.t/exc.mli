type 'a queue
(*@ mutable model : 'a sequence *)

val pop : 'a queue -> 'a
(*@ modifies q
    match pop q with
    | x -> ensures Sequence.cons x q = old q
    | exception Not_found ->
        produces q
        ensures q = old q = Sequence.empty *)
