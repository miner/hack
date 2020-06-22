(ns miner.shortcirc)

;; https://rosettacode.org/wiki/Short-circuit_evaluation#Clojure

;; Current
(defn rc-short-circ []
  (letfn [(a [bool] (print "(a)") bool)   
          (b [bool] (print "(b)") bool)]  
    (doseq [i [true false] j [true false]]
      (print i "OR" j "= ")               
      (println (or (a i) (b j)))          
      (print i "AND" j " = ")             
      (println (and (a i) (b j))))))


(defn short-circ []
  (let [test? (fn [tag x] (when tag (print tag)) (true? x))
        a (partial test? "A")
        b (partial test? "B")
        and-results (for [i [true false]
                          j [true false]]
                        {'i i 'j j
                         "i AND j" (and (test? nil i) (test? nil j))
                         'print (with-out-str (and (a i) (b j)))})
        or-results (for [i [true false]
                         j [true false]]
                       {'i i 'j j
                        "i OR j" (or (test? nil i) (test? nil j))
                        'print (with-out-str (or (a i) (b j)))})]
    (clojure.pprint/print-table and-results)
    (clojure.pprint/print-table or-results))
  (println))


