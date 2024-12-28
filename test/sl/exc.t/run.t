  $ gospel check --sep exc.mli
  Predicate ∀ 'a. Queue (target : 'a queue loc) (model : 'a sequence)
  
  Triple pop :
  ∀ 'a. (_prog_q : 'a queue loc), (q : 'a sequence). 
  { Queue(_prog_q, q) }
  pop _prog_q 
  { λ (x : 'a).
    ∃ (_q_ : 'a sequence).
      Queue(_prog_q_1, _q_) * [((cons  x _q_) = q)] }
  { Not_found -> Queue(_prog_q_2, _q__1) * [(_q__1 = q) /\ (q = (empty ))]}
