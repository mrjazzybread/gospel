type 'a structure = Tyapp of Ident.t * 'a list | Tyarrow of 'a * 'a

val create_id : string -> Ident.t
val bool_id : Ident.t
val integer_id : Ident.t
val char_id : Ident.t
val string_id : Ident.t
val float_id : Ident.t
val sequence_id : Ident.t
val ty_bool : 'a structure
val ty_integer : 'a structure
val ty_char : 'a structure
val ty_string : 'a structure
val ty_float : 'a structure
val primitive_list : (string * Ident.t) list
val ty_arrow : 'a -> 'a -> 'a structure
val iter : ('a -> unit) -> 'a structure -> unit
val fold : ('a -> 'b -> 'b) -> 'a structure -> 'b -> 'b
val map : ('a -> 'b) -> 'a structure -> 'b structure

exception InconsistentConjunction

val conjunction :
  ('a -> 'b -> unit) -> 'a structure -> 'b structure -> 'a structure

val pprint : 'a -> 'b -> 'c
