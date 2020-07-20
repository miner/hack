;;; Eric challenge.  indices-of
;;; https://gist.github.com/ericnormand/99c57ed413cc280ce72d6094868fef75

(ns miner.eindices)

;; Jeroen solution
;; extended to transducer (good idea)
;; asked about duplication?

(defn indices-of
  "Returns a lazy sequence of all indices in (seq `coll`) at which `x` was found.
   Returns a stateful transducer when no collection is provided."
  ([x]
   (keep-indexed (fn [idx el]
                   (when (= x el) idx))))
  ([x coll]
   (keep-indexed (fn [idx el]
                   (when (= x el) idx))
                 coll)))

(defn smoke-ind [indices-of]
  (assert (= []    (indices-of :d [])))
  (assert (= []    (indices-of :d [0 1 2])))
  (assert (= [0]   (indices-of :d [:d])))
  (assert (= [2 6] (indices-of :d [:a 1 :d :f :r 5 :d])))
  (assert (= [1000] (indices-of 1000 (range 2000))))
  true)


;; SEM two-arg should call transducer version

(defn indof
  ([x]   (keep-indexed (fn [idx el] (when (= x el) idx))))
  ([x coll]   (sequence (indof x) coll)))

