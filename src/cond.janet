(defmacro cond
  `Custom implementation of COND that supports a => form, which allows for
   binding against the result of the predicate.`
  [& clauses]
  (defn aux [clauses]
    (match clauses
      [pred '=> binder body & rest]
      ~(if-let [,binder ,pred] ,body ,(aux rest))
      [pred body & rest]
      ~(if ,pred ,body ,(aux rest))
      [default] default
      [] 'nil))
  (aux clauses))
