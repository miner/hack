(ns miner.varfn)

(defn square
  {:domain Double/TYPE
   :codomain Double/TYPE
   :range {:from 0.0 :to Double/POSITIVE_INFINITY :also Double/NaN}}
  ([] (meta #'square))
  (^double [^double x] (* x x)))





