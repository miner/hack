;; for convenient comparison and reductions

(defn same
  ([] nil)
  ([a] a)
  ([a b] (when (= a b) a))
  ([a b c] (when (= a b c) a))
  ([a b c d] (when (= a b c d) a))
  ([a b c d & more] (when (apply = a b c d more) a)))


