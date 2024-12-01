module M : sig
  type t1
  
  type t2
  (*@ ephemeral *)


  module N : sig
    type t3
    (*@ mutable model : integer *)

    type t4
    (*@ mutable model n : integer *)
  end

end
