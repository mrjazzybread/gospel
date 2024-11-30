  $ gospel check --sep type_def.mli
  Type no_model
  
  Predicate Eph (target : eph loc) (model : eph loc)
  
  Type machine_int
  
  Pure Predicate Machine_int (target_1 : machine_int) (model_1 : integer)
  
  Type _int_list
  
  Type int_list = { elems : integer sequence }
  
  Pure Predicate Int_list (target_2 : _int_list) (model_2 : int_list)
  
  Type ∀ 'a. _ocaml_sequence
  
  Type ∀ 'a. ocaml_sequence = { size : integer; list : 'a sequence }
  
  Pure Predicate ∀ 'a. Ocaml_sequence (target_3 : 'a _ocaml_sequence)
                                        (model_3 : 'a ocaml_sequence)
  
  Predicate Int_ref (target_4 : int_ref loc) (model_4 : integer)
  
  Type int_dyn_array = { eph_elems : integer sequence }
  
  Predicate Int_dyn_array (target_5 : _int_dyn_array loc)
                          (model_5 : int_dyn_array)
  
  Type ∀ 'a. static_array = { eph_size : integer; eph_list : 'a sequence }
  
  Predicate ∀ 'a. Static_array (target_6 : 'a _static_array loc)
                                 (model_6 : 'a static_array)
