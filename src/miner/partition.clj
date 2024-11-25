(ns miner.partition)

;;; euler08.clj solution makes me think it would be nice to have a partition variant that
;;; returns subvectors.  Fast if you have an original vector.

;;; Good
(defn subpartitionv
  ([n v] (subpartitionv n n v))
  ([n step v]
   (map #(subvec v % (+ % n)) (range 0 (- (count v) (dec n)) step)))
  ([n step pad v]
   (let [cnt (count v)]
     (cond (zero? cnt) ()
           (<= cnt n) (list (into v (take (- n cnt)) pad))
           :else (concat (subpartitionv n step v)
                         (lazy-seq
                          (let [padlen (- step (rem (- cnt n) step))]
                            (when (< padlen n)
                              (list (into (subvec v (- cnt (- n padlen)))
                                          (take padlen) pad))))))))))

;;; lazy-seq doesn't seems to help much with just the last item but it doesn't hurt



(def v20 (vec (range 20)))
(defn test-subp [subp]
  (let [pad [-1 -2 -3 -4 -5]]
    (assert (= (partitionv 3 1 pad []) (subpartitionv 3 1 pad [])))
    (assert (= (partitionv 3 1 pad [1]) (subpartitionv 3 1 pad [1])))
    (assert (= (partitionv 3 1 pad v20) (subpartitionv 3 1 pad v20)))
    (assert (= (partitionv 3 2 pad v20) (subpartitionv 3 2 pad v20)))
    (assert (= (partitionv 3 2 nil v20) (subpartitionv 3 2 nil v20)))
    (assert (= (partitionv 3 12 pad v20) (subpartitionv 3 12 pad v20)))
    true))


(defn test-subp1 [subp]
  (let [pad [-1 -2 -3 -4 -5]]
    (assert (= (first (partitionv 3 1 pad [])) (first (subpartitionv 3 1 pad []))))
    (assert (= (first (partitionv 3 1 pad [1])) (first (subpartitionv 3 1 pad [1]))))
    (assert (= (first (partitionv 3 1 pad v20)) (first (subpartitionv 3 1 pad v20))))
    (assert (= (first (partitionv 3 2 pad v20)) (first (subpartitionv 3 2 pad v20))))
    (assert (= (first (partitionv 3 2 nil v20)) (first (subpartitionv 3 2 nil v20))))
    (assert (= (first (partitionv 3 12 pad v20)) (first (subpartitionv 3 12 pad v20))))
    true))

(defn test-rsubp [subp]
  (dotimes [i 1000]
    (let [v (vec (range (+ 20 (rand-int 10))))
          n (inc (rand-int 19))
          step (inc (rand-int 19))
          pad (range -1 (- step) -1)]
      (assert (= (partition n step pad v) (subp n step pad v)))))
  true)



(defn padcnt [n step cnt]
  (cond (zero? cnt) 0
        (< cnt n) (- n cnt)
        :else (let [r (rem (- cnt n) step)
                    p (- step r)
                    pd (when (< p n) p)]
                [cnt r p pd])))



;;;; QUESTION  what's the diff between (partitionv-all ...) and (partitionv with nil pad)
;;;; ???
;;; ask Clojure
        
(defn pv [n step cnt]
  (last (partitionv n step (range 1 (inc step)) (vec (repeat cnt 0)))))


(defn pstepcnt [step cnt]
  (into [[:step step :cnt cnt]]
        (for [n (range 1 (+ cnt step))]
          (cond (zero? cnt) 0
                (< cnt n) (- n cnt)
                :else (let [r (rem (- cnt n) step)
                            p (- step r)]
                        (when (< p n) p))))))

(defn pncnt [n cnt]
  (into [[:n n :cnt cnt]]
        (for [step (range 1 (* 2 n))]
          (cond (zero? cnt) 0
                (< cnt n) (- n cnt)
                :else (let [r (rem (- cnt n) step)
                            p (- step r)]
                        (when (< p n) p))))))
