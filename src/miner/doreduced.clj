;; http://dev.clojure.org/jira/browse/CLJ-1523

(defmacro doreduced
  "For every value in coll, execute the transducer, and bind the resulting value to
  the given binding. Prefer this macro over doseq when transducers are in use.

  Example:
    (doreduced [b xform coll]
      .. do something with b ..)"
  {:added "1.7"}
  [[binding-name xform coll] & body]
  (if coll
    `(transduce
      ~xform
      (fn
        ([] nil)
        ([result#] nil)
        ([result# ~binding-name]
         ~@body
         nil))
      ~coll)
    `(reduce
      (fn
        ([result# ~binding-name]
          ~@body
          nil))
      ~xform)))


(defn test-doreduced []
  (and 
   (let [a (atom 0)]
     (doreduced [x (map inc) (range 10)]
                (swap! a + x))
     (= @a 55))
   (let [a (atom 0)]
     (doreduced [x (range 10)]
                (swap! a + x))
     (= @a 45))))


(defn test-doseq []
  (and 
   (let [a (atom 0)]
     (doseq [x (eduction (map inc) (range 10))]
       (swap! a + x))
     (= @a 55))
   (let [a (atom 0)]
     (doseq [x (range 10)]
       (swap! a + x))
     (= @a 45))))

