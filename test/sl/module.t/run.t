  $ gospel check --sep module.mli
  Module M :
    Type t1
    Predicate T2 (target : t2 loc) (model : t2 loc)
    Module N :
      Predicate T3 (target_1 : t3 loc) (model_1 : integer)
      Type t4 = { n : integer }
      Predicate T4 (target_2 : _t4 loc) (model_2 : t4)
    end
  end
