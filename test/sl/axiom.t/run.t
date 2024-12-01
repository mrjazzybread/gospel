  $ gospel check --sep axiom.mli 
  Axiom add_ge :
  [forall x:integer : integer @ integer y:integer : integer @ integer. ((
  x + y) >= y) && ((x + y) >= x)]
  
  Predicate T (target : t loc) (model : integer)
  
  Axiom t_0 :
  ∀ (_prog_t : integer), (t_1 : integer). T(_prog_t, t_1) -*> [(t_1 = 0)] *
                                                                T(_prog_t, t_1)]
