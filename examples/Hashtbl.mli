module type H = sig
  (*@ open Sequence *)
  (*@ open Map *)

  type ('a, 'b) t
  (*@ mutable model : 'a -> 'b sequence *)

  val create : int -> ('a, 'b) t
  (*@ let tbl = create n in
        produces tbl @ ('a, 'b) t
        ensures tbl = fun _ -> empty *)

  val clear : ('a, 'b) t -> ('a, 'b) t
  (*@ modifies tbl @ ('a, 'b) t
      let _ = clear tbl in
        ensures tbl = fun _ -> empty *)

  val copy : ('a, 'b) t -> ('a, 'b) t
  (*@ preserves tbl @ ('a, 'b) t
      let c = copy tbl in
        ensures tbl = c *)

  val add : ('a, 'b) t -> 'a -> 'b -> unit
  (*@ modifies tbl @ ('a, 'b) t
      let _ = add tbl k v in
        ensures tbl = old (tbl[k -> cons v (tbl k)]) *)

  val find_opt : ('a, 'b) t -> 'a -> 'b option
  (*@ preserves tbl @ ('a, 'b) t
      let r = find_opt tbl k in
      ensures match r with
        |None -> tbl k = empty
        |Some x -> hd (tbl k) = x *)

  val find_all : ('a, 'b) t -> 'a -> 'b list
  (*@ preserves tbl @ ('a, 'b) t
      let l = find_all tbl k in
        ensures l = tbl k *)

  val mem : ('a, 'b) t -> 'a -> bool
  (*@ preserves tbl @ ('a, 'b) t
      let b = mem tbl k in
        ensures b <-> tbl k <> empty *)

  val remove : ('a, 'b) t -> 'a -> unit
  (*@ modifies tbl @ ('a, 'b) t
      let _ = remove tbl k in
        ensures old (tbl k) = empty -> tbl = old tbl
        ensures tbl k <> empty -> tbl = old (tbl[k -> tl (tbl k)]) *)

  val replace : ('a, 'b) t -> 'a -> 'b -> unit
  (*@ modifies tbl @ ('a, 'b) t
      let _ = replace tbl k v in
        ensures tbl k = empty -> tbl = tbl[k -> singleton v]
        ensures tbl k <> empty ->
        let tail = old (tl (tbl k)) in
        tbl = old (tbl[k -> cons v tail]) *)

  (*@ function domain (default : 'b) (m : 'a -> 'b) : 'a Set.t *)
  (*@ axiom domain_mem :
        forall x m default. m x <> default -> Set.mem x (domain default m) *)

  val length : ('a, 'b) t -> int
  (*@ preserves tbl @ ('a, 'b) t
      let n = length tbl in
        ensures n = Set.cardinal (domain empty tbl) *)
end
