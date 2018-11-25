(ns miner.generators)

;; Borrowed code from Ghadi Shayban
;; https://gist.github.com/ghadishayban/902373e247e920855139902912d237f0
;;
;; Re: CLJ-1906 Clojure should make representing iterated api calls easier
;; https://dev.clojure.org/jira/browse/CLJ-1906



; differences from scheme unfold
;; even initial value is lazy
;; predicate sense reversed
;; internal state == produced value, no special mapper-fn
;; no tail-gen
(defn series
  "Produces a sequence of values.

   `f` is a function that given a value, returns the next value.
   `continue?` is a predicate that determines whether to produce
    the next value. `f` called with no arguments produces the
    initial value. Always produces at least one value."
  [f continue?]
  (reify
    clojure.lang.Seqable
    (seq [_]
      ((fn step [seed]
         (cons seed
               (when (continue? seed)
                 (lazy-seq (step (f seed))))))
       (f)))
    clojure.lang.IReduceInit
    (reduce [_ rf init]
       (loop [seed (f)
             ret (rf init seed)]
        (if (reduced? ret)
          @ret
          (if (continue? seed)
            (let [next (f seed)]
               (recur next (rf ret next)))
            ret))))
    clojure.lang.Sequential))


(defn supply
  "Produces a sequence of values by repeatedly calling `f` for side-effects
    until it returns `fin`. The sequence can be used lazily/caching or
   reducible/non-caching. Not guaranteed to produce any values"
  [f fin]
  (reify
    clojure.lang.Seqable
    (seq [_]
      ((fn step []
         (let [v (f)]
           (if (identical? v fin)
             nil
             (cons v (new clojure.lang.LazySeq step)))))))
    clojure.lang.IReduceInit
    (reduce [_ rf init]
      (loop [res init]
        (let [v (f)]
          (if (identical? v fin)
            res
            (let [res (rf res v)]
              (if (reduced? res)
                @res
                (recur res)))))))
    clojure.lang.Sequential))




(comment

  ;; supply
  (supply #(or (.readLine ^BufferedReader rdr) ::eof) ::eof)

  ;; series
  (let [api (fn ([]
                 (GET "/initial"))
              ([response]
               (GET (next-link response))))]
    (series api next-link))

)


  
