(ns miner.recursion)

;; https://purelyfunctional.tv/issues/purelyfunctional-tv-newsletter-362-tip-double-recursion/

;; Original from Eric
(defn print-double-nested
  "v is a doubly nested vector, like [[1 2 3] [4 5 6]]"
  [v]
  (loop [inner [] outer v]
    (cond
      (not (empty? inner))
      (let [[f & rst] inner]
        (println f)
        (recur rst outer))

      (not (empty? outer))
      (let [[f & rst] outer]
        (recur f rst))
      
      :else
          :done!)))



;; SEM comments
;; don't use (not (empty?...)), see empty? doc-string.

(defn pdn
  "v is a doubly nested vector, like [[1 2 3] [4 5 6]]"
  [v]
  (loop [inner [] outer v]
    (cond
     (seq inner) (let [[f & rst] inner]
                   (println f)
                   (recur rst outer))
     (seq outer) (let [[f & rst] outer]
                   (recur f rst))
     :else :done!)))



(defn ldn
  "v is a doubly nested vector, like [[1 2 3] [4 5 6]]"
  [v]
  (loop [outer (seq v)]
    (when outer
      (loop [inner (first outer)]
        (when inner
          (println (first inner))
          (recur (next inner))))
      (recur (next outer))))
  :done!)

(def vvv [[1 2 3] [4 5 6]])


(defn fdn [v]
  (run! println
        (for [row v x row] x))
  :done!)


(defn fdn [v]
  (dorun (for [row v x row] (println x)))
  :done!)
