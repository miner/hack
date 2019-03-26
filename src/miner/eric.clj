(ns miner.eric)
  
;; slighlty hacked to fix arg order
(defn mark-combinations [items n]
  (cond
    (= n 0) #{#{}}
    (empty? items) #{}
    :else
    (concat (map
             #(clojure.set/union (hash-set (first items)) %)
             (mark-combinations (rest items) (dec n)))
            (mark-combinations (rest items) n))))




(defn sem-combinations [coll n]
  (let [choose-indices (fn [cnt choose]
                         ;; {:pre [(pos-int? choose) (<= choose cnt)]}
                         (loop [i (dec choose) res (map vector (range cnt))]
                           (if (zero? i)
                             res
                             (recur (dec i)
                                    (mapcat (fn [prev] (map #(conj prev %)
                                                            (range (inc (peek 
                                                                         prev)) cnt)))
                                            res)))))]
    (if-not (pos-int? n)
      (list ())
      (let [v (vec coll)
            cnt (count v)]
        (when (<= n cnt)
          (map #(into #{} (map v) %) (choose-indices cnt n)))))))



(defn combinations' [r coll n]
  (cond
    (> n (count coll))
    nil

    (zero? n)
    nil

    (= 1 n)
    (map list coll)

    :else
    (let [x (first coll)
          xs (rest coll)]
      (lazy-cat
       ;; combinations with first element
       (map #(conj % x) (r r xs (dec n)))
       ;; combinations without first element
       (r r xs n)))))

(defn eric-combinations [coll n]
  (let [f (memoize combinations')]
    (f f coll n)))

(defn set=
  ([] true)
  ([a] true)
  ([a b] (= (set a) (set b)))
  ([a b & more] (and (set= a b) (apply set= more))))




