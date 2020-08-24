(ns miner.ebinsearch)

;; https://gist.github.com/ericnormand/892bcb31280859727c2375c7d41bea81

;; You can assume you're passed a sorted vector.



(defn binary-search [x vord]
  (loop [lo 0  hi (count vord)]
    (and (< lo hi)
         (let [i (quot (+ hi lo) 2)
               v (vord i)]
           (cond (= x v) true
                 (> v x) (recur lo i)
                 :else (recur (inc i) hi))))))




;; slightly slower
(defn binsearch
  ([x vord] (binsearch x vord 0 (count vord)))
  ([x vord lo hi]
   (when (< lo hi)
     (let [i (quot (+ hi lo) 2)
           v (vord i)]
       (cond (= x v) true
             (> v x) (recur x vord lo i)
             :else (recur x vord (inc i) hi))))))




(defn smoke-bin [binary-search]
  (assert (binary-search 3 [3]))
  (assert (not (binary-search 1 [3])))
  (assert (not (binary-search 1 [])))
  (assert (binary-search 3 [1 2 3]))
  (assert (not (binary-search 4 [1 2 5])))
  (assert (binary-search 10 [1 2 4 5 9 10 11 12]))
  (let [vk (vec (range 1000))]
    (assert (binary-search 0 vk))
    (assert (binary-search 500 vk))
    (assert (binary-search 999 vk))
    (assert (not (binary-search 1000 vk))))
  true)



;; naive
(defn search [x vord]
  (reduce (fn [res v]
            (cond (= v x) (reduced true)
                  (> v x) (reduced false)
                  :else false))
          false
          vord))

(defn search1 [x vord]
  (reduce (fn [res v] (when (= x v) (reduced true))) false vord))


(defn search2 [x vord]
  (reduce (fn [res v]
            (let [c (compare v x)]
              (cond (zero? c) (reduced true)
                    (neg? c) false
                    :else (reduced false))))
          false
          vord))


;; OK, but not nearly as fast
(defn dsearch [x vord]
  (= x (first (sequence (comp (drop-while #(< % x)) (take 1)) vord))))


;; slowish
(defn searchd [x vord]
  (= x (first (drop-while #(< % x) vord))))

(defn dsearch1 [x vord]
  (= x (first (sequence (drop-while #(< % x)) vord))))

(defn dsearch3 [x vord]
  (= x (peek (into [] (comp (drop-while #(< % x)) (take 1)) vord))))




(defn binary-search-cmp [x vord]
  (loop [lo 0  hi (count vord)]
    (when (< lo hi)
      (let [i (quot (+ hi lo) 2)
            cmp (compare x (vord i))]
        (cond (zero? cmp) true
              (neg? cmp) (recur lo i)
              :else (recur (inc i) hi))))))

