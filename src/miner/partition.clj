(ns miner.partition)


;;; euler08.clj solution makes me think it would be nice to have a partition variant that
;;; returns subvectors.  Fast if you have an original vector.


(defn subpartitionv1
  ([n v] (subpartitionv1 n n v))
  ([n step v]
   (map #(subvec v % (+ % n)) (range 0 (- (count v) (dec n)) step)))
  ([n step pad v]
   (concat (subpartitionv1 n step v)
           (let [cnt (count v)
                 pcnt (cond (zero? cnt) 0
                            (< cnt n) (- n cnt)
                            :else  (- step (rem (- cnt n) step)))]
             (when (< pcnt n)
               (list (into (subvec v (- cnt (- n pcnt))) (take pcnt) pad)))))))

;;; FIXME:  (pos? padlen) probably not necessary

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

(defn subpartitionv3
  ([n v] (subpartitionv3 n n v))
  ([n step v]
   (map #(subvec v % (+ % n)) (range 0 (- (count v) (dec n)) step)))
  ([n step pad v]
   (let [cnt (count v)]
     (cond (zero? cnt) ()
           (<= cnt n) (list (into v (take (- n cnt)) pad))
           :else (concat (subpartitionv3 n step v)
                         (let [padlen (- step (rem (- cnt n) step))]
                           (when (< padlen n)
                             (lazy-seq
                              (list (into (subvec v (- cnt (- n padlen)))
                                          (take padlen) pad))))))))))

;;; lazy-seq doesn't help much with just the last item
(defn subpartitionv4
  ([n v] (subpartitionv4 n n v))
  ([n step v]
   (map #(subvec v % (+ % n)) (range 0 (- (count v) (dec n)) step)))
  ([n step pad v]
   (let [cnt (count v)]
     (cond (zero? cnt) ()
           (<= cnt n) (list (into v (take (- n cnt)) pad))
           :else (concat (subpartitionv4 n step v)
                         (let [padlen (- step (rem (- cnt n) step))]
                           (when (< padlen n)
                              (list (into (subvec v (- cnt (- n padlen)))
                                          (take padlen) pad)))))))))


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


;;; testing only pad usage
(defn padcnt [n step cnt]
  (cond (zero? cnt) 0
        (< cnt n) (- n cnt)
        :else (- step (rem (- cnt n) step))))

(defn pcnt [n step cnt]
  (cond (zero? cnt) 0
        (< cnt n) (- n cnt)
        :else (let [r (rem (- cnt n) step)
                    p (- step r)
                    pd (when (and (pos? p) (< p n)) p)]
                [cnt r p pd])))



;;;; QUESTION  what's the diff between (partitionv-all ...) and (partitionv with nil pad)
;;;; ???
;;; ask Clojure
        
(defn pv [n step cnt]
  (last (partitionv n step (vec (range -1 (- n) -1)) (vec (range cnt)))))

