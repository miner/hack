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
;;;; ??? -- actually different for some cases.  See below.   but maybe a bug??
;;; ask Clojure
;;; lots of partition variants -all, *v
;;; not counting partition-by

(comment
clojure.core/partition
([n coll] [n step coll] [n step pad coll])

clojure.core/partition-all
([n] [n coll] [n step coll])

clojure.core/partitionv
([n coll] [n step coll] [n step pad coll])

clojure.core/partitionv-all
([n] [n coll] [n step coll])

;; end comment
)


;;; My take is that there are historical reasons for some of the variants.  It might have
;;; been done differently if Clojure were starting for scratch.
;;; partitionv-all seems like it mostly supersedes partition-all.

;;; doc string on partition-all says it produces lists, but the transducer arity produces
;;; vectors -- which explains why partitionv-all just calls into the original.

;;; no padding option on the -all variants.  Seems like pad of nil would give you the -all
;;; result.  Actually, not true!  n=14, step=7, cnt=22 is counterexample


(comment 

;;; Issuse seems to be when N > 2*STEP.  Just the way it is.  Not likely to change, just
;;; something to be aware of.
  

(partition-all 5 2 (range 10))
((0 1 2 3 4) (2 3 4 5 6) (4 5 6 7 8) (6 7 8 9) (8 9))
;; notice second to last partition (6 7 8 9) has only four elements.  Why not stop there?

(partition 5 2 nil (range 10))
((0 1 2 3 4) (2 3 4 5 6) (4 5 6 7 8) (6 7 8 9))



(partition-all 3 1  (range 5))
((0 1 2) (1 2 3) (2 3 4) (3 4) (4))
;; notice second to last (3 4) partition.  Why not stop there?

;; nil pad gives me the expected result
(partition 3 1 nil  (range 5))
((0 1 2) (1 2 3) (2 3 4) (3 4))


;;; more dramatic example. 
(map count (partition-all 7 1  (range 10)))
(7 7 7 7 6 5 4 3 2 1)

;;; my preference
(map count (partition 7 1 nil (range 10)))
(7 7 7 7 6)

;;; as expected
(map count (partition 7 1 (range 10)))
(7 7 7 7)


;; interested progression of tails
(map count (partition-all 7 7 (range 10)))
(7 3)

(map count (partition-all 7 6 (range 10)))
(7 4)

(map count (partition-all 7 5 (range 10)))
(7 5)

(map count (partition-all 7 4 (range 10)))
(7 6 2)

(map count (partition-all 7 3 (range 10)))
(7 7 4 1)

(map count (partition-all 7 2 (range 10)))
(7 7 6 4 2)

(map count (partition-all 7 1 (range 10)))
(7 7 7 7 6 5 4 3 2 1)



(partition-all 5 7 (range 20))
((0 1 2 3 4) (7 8 9 10 11) (14 15 16 17 18))
;; seems strange at first but 19 would have been skipped by step so this is right.

;; end comment
)

(defn demo7 []
  (let [r (range 1 10)]
    (doseq [i r]
      (println)
      (println "(map count (partition-all 7" i "(range 1 10)))")
      (println (map count (partition-all 7 i r)))
      (println "(map count (partition 7" i "nil (range 1 10)))")
      (println (map count (partition 7 i nil r)))))
  (println))

(defn test-pall []
  (dotimes [i 10]
    (let [v (vec (range (+ 20 (rand-int 10))))
          n (inc (rand-int 19))
          step (inc (rand-int 19))]
      (assert (= (partition-all n step v) (partition n step nil v))
              (pr-str [n step (count v)]))
      (println n step (count v))))
  true)


        
(defn pv [n step cnt]
  (last (partitionv n step (range 1 (inc step)) (vec (repeat cnt 0)))))


;;; looking for a more precise pattern in the padlen

;;; varies n from 1 for fixed step and cnt
(defn pstepcnt [step cnt]
  (into [[:step step :cnt cnt]]
        (for [n (range 1 (+ cnt step))]
          (cond (zero? cnt) 0
                (< cnt n) (- n cnt)
                :else (let [r (rem (- cnt n) step)
                            p (- step r)]
                        (when (< p n) p))))))

;;; varies step from 1 to 2n for fixed n and cnt
(defn pncnt [n cnt]
  (into [[:n n :cnt cnt]]
        (for [step (range 1 (* 2 n))]
          (cond (zero? cnt) 0
                (< cnt n) (- n cnt)
                :else (let [r (rem (- cnt n) step)
                            p (- step r)]
                        (when (< p n) p))))))
