  $ gospel check --sep fun_def.mli
  Function add (x : integer) (y : integer) : integer
  
  Function add_impl (x_1 : integer) (y_1 : integer) : integer=
   (x_1 + y_1)
  
  Function hd ∀ 'a. (s : 'a sequence) : 'a
  
  Function hd_impl ∀ 'a. (s_1 : 'a sequence) : 'a=
   (mixfix [_]  s_1 0)
  
  Function is_empty ∀ 'a. (s_2 : 'a sequence) : bool
  
  Function is_empty_impl ∀ 'a. (s_3 : 'a sequence) : bool=
   (s_3 = (empty ))
