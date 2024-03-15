(*@ open Sequence *)

type t
(*@ mutable model : integer sequence *)

val create : unit -> t
(*@ arr = create () 
    ensures arr = empty *)

val make : int -> int -> t
(*@ arr = make n x
   ensures arr = init n (fun _ -> x) *)

val get : t -> int -> int
(*@ x = get arr i
    ensures x = arr[i] *)

val set : t -> int -> int -> unit
(*@ set arr i x
   modifies arr @ t 
   ensures arr = set (old arr) i x *)

val length : t -> int
(*@ l = length arr 
    ensures l = Sequence.length arr *)

val is_empty : t -> bool
(*@ b = is_empty arr
    ensures b <-> arr = empty *)

val find_last : t -> int option
(*@ x = find_last arr 
    ensures match x with
    |None -> arr = empty
    |Some x -> arr[length arr - 1] = x *)

val copy : t -> t
(*@ cp = copy arr
    ensures cp = arr *)

val add_last : t -> int -> unit
(*@ add_last arr x
    modifies arr
    ensures arr = set (old arr) (length arr - 1) x *)

val append : t -> t -> unit
(*@ append arr1 arr2
    modifies arr1
    ensures arr1 = (old(arr1) ++ arr2) *)
