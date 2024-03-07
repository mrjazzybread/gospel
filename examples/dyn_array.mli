(*@ open Sequence *)

type t
(*@ mutable model : int sequence *)

val make : int -> int -> t
(*@ arr = make size dummy
    ensures init size (fun _ -> dummy) *)

val create : unit -> t
(*@ arr = create () 
    ensures arr = empty *)

val resize : t -> int -> unit
(*@ resize arr n
    modifies arr @ t
    ensures
      let diff = length arr - (old (length arr)) in 
      if diff >= 0
        then arr = (old arr) ++ init diff (fun _ -> arr.dummy) 
        else arr = old(arr[.. length arr + diff])
