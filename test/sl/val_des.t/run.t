  $ gospel check --sep vd.mli
  Triple add :
  ∀(x : int), (y : int). 
  {  }
  add x y 
  { λ (z : int).
      [((integer_of_int  z) = ((integer_of_int  x) + (integer_of_int  y)))] }
  
  Type elt
  
  Triple st_eq :
  ∀(x_1 : elt), (y_1 : elt). 
  {  }
  st_eq x y 
  { λ (b_1 : bool).
      [b_1 <-> (x_1 = y_1)] }
  
  Triple identity :
  ∀(__arg0 : elt). 
  {  }
  identity __arg0 
  {  }
  
  Predicate ∀ 'a. Pointer (target : 'a pointer loc) (model : 'a)
  
  Triple get :
  ∀ 'a. (_prog_p : 'a pointer loc), (p : 'a). 
  { Pointer(_prog_p, p) }
  get _prog_p 
  { λ (r : 'a).
      Pointer(_prog_p_1, p) * [(r = p)] }
  
  Triple set_1 :
  ∀ 'a. (_prog_p_2 : 'a pointer loc), (p_1 : 'a), (x_2 : 'a). 
  { Pointer(_prog_p_2, p_1) }
  set _prog_p x 
  {   Pointer(_prog_p_3, x_2) }
  
  Triple ph_eq :
  ∀ 'a. (x_3 : 'a pointer loc), (y_2 : 'a pointer loc). 
  {  }
  ph_eq x y 
  { λ (b_2 : bool).
      [b_2 <-> (x_3 = y_2)] }
  
  Type ∀ 'a. array = { size : int; elts : 'a sequence }
  
  Predicate ∀ 'a. Array (target_1 : 'a _array loc) (model_1 : 'a array)
  
  Triple set_i :
  ∀ 'a. (_prog_arr : 'a array loc), (arr : 'a array), (i : int), (x_4 : 'a). 
  { Array(_prog_arr, arr) }
  set_i _prog_arr i x 
  { ∃ (_arr_ : 'a array).
      Array(_prog_arr_1, _arr_) *
      [((_arr_).elts = (set_2  (arr).elts (integer_of_int  i) x_4))] }
  
  Triple concat :
  ∀ 'a. (_prog_arr1 : 'a array loc), (arr1 : 'a array),
          (_prog_arr2 : 'a array loc), (arr2 : 'a array). 
  { Array(_prog_arr1, arr1) * Array(_prog_arr2, arr2) }
  concat _prog_arr1 _prog_arr2 
  { λ (_prog_r : 'a array loc).
    ∃ (r_1 : 'a array).
      Array(_prog_arr1_1, arr1) * Array(_prog_arr2_1, arr2) *
      Array(_prog_r, r_1) * [((r_1).elts = ((arr1).elts ++ (arr2).elts))] }
