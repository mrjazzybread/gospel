type no_model
type eph
(*@ ephemeral *)

type machine_int
(*@ model : integer *)

type int_list
(*@ model elems : integer sequence *)

type 'a ocaml_sequence
(*@ model list : 'a sequence
    model size : integer *)

type int_ref
(*@ mutable model : integer *)

type int_dyn_array
(*@ mutable model eph_elems : integer sequence *)

type 'a static_array
(*@ mutable model eph_list : 'a sequence
    model eph_size : integer *)
(* {gospel_expected|
   Type no_model

   Predicate Eph (target : loc)  (model : loc)

   Type machine_int

   Predicate Machine_int (target_1 : machine_int)  (model_1 : integer)

   Type _int_list

   Type int_list = { elems : integer sequence }

   Predicate Int_list (target_2 : _int_list)  (model_2 : int_list)

   Type 'a _ocaml_sequence

   Type 'a ocaml_sequence = { size : integer; list : 'a sequence }

   Predicate Ocaml_sequence (target_3 : 'a _ocaml_sequence)
                            (model_3 : 'a ocaml_sequence)

   Predicate Int_ref (target_4 : loc)  (model_4 : integer)

   Type int_dyn_array = { eph_elems : integer sequence }

   Predicate Int_dyn_array (target_5 : loc)  (model_5 : int_dyn_array)

   Type 'a static_array = { eph_size : integer; eph_list : 'a sequence }

   Predicate Static_array (target_6 : loc)  (model_6 : 'a static_array)
   |gospel_expected} *)
