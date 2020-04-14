(ns miner.tester)

;; useful for quick testing, not necessarily for production
;; most people would use clojure.test but it's a bit more that I need in the REPL


(defmacro mkasserts
  ([eq] true)
  ([eq form expected & more]
   `(let [form# ~form]
      (if-not (~eq form# ~expected)
        (throw (new AssertionError (str "Failed on " (pr-str '~form)
                                      ", returned " (pr-str form#)
                                      ", expected " (pr-str '~expected))))
        (mkasserts ~eq ~@more)))))

;; EQ is normally =.  For some numerics, == would be appropriate.  For some collections,
;; maybe set= (defined below) is better if you don't care about order.
;; Note: returns true if all the equivalencies are true.

(defmacro asserting
  ([eq & forms+expecteds]
   `(let [eq# ~eq]
      (mkasserts eq# ~@forms+expecteds))))

;; when order in coll is not significant
(defn set= [a b]
  (and (= (count a) (count b))
       (= (set a) (set b))))


(comment

  (defn smoke-fn
    ([] (smoke-fn defaultf))
    ([f] (asserting =
           (f 15 2) 30
           (f x) y)))

  (asserting ==
     (+ 3.0 4.0) 7
     (- 4 3) (- 2 1))
  
  )
