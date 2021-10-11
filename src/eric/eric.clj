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

;;;----------------------------------------------------------------------
;;; SEM re-working

;;; Eric is using a sort of integrated Y-combinator to get memoized recursion. The memo
;;; cache is localized per top-level call, which is a good idea to control memory usage.  Of
;;; course, that puts an extra requirement on the base function that it must take its own
;;; "memoization" as the first argument, and "recursive" calls must use that arg instead of
;;; literal recursion.

(defn rec-memo [fnr]
  (let [mf (memoize fnr)]
    (partial mf mf)))


(defn ecombo1 [r coll n]
  (cond
    (zero? n)    '(())
    (> n (count coll)) nil

    (= 1 n)  (map list coll)

    :else    (let [x (first coll)
                   xs (rest coll)]
               (lazy-cat
                ;; combinations with first element
                (map #(conj % x) (r r xs (dec n)))
                ;; combinations without first element
                (r r xs n)))))

(defn ecombo [coll n]
  (let [f (rec-memo ecombo1)]
    (f coll n)))


(defn set=
  ([] true)
  ([a] true)
  ([a b] (= (set a) (set b)))
  ([a b & more] (and (set= a b) (apply set= more))))



(defn my-combinations [coll n]
  (let [choose-indices (fn [cnt choose]
                        ;; {:pre [(pos-int? choose) (<= choose cnt)]}
                        (loop [i (dec choose) res (map vector (range cnt))]
                          (if (zero? i)
                            res
                            (recur (dec i)
                                   (mapcat (fn [prev] (map #(conj prev %)
                                                           (range (inc (peek prev)) cnt)))
                                           res)))))]
    (if-not (pos-int? n)
      (list ())
      (let [v (vec coll)
            cnt (count v)]
        (when (<= n cnt)
          (map #(map v %) (choose-indices cnt n)))))))

