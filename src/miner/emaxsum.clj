(ns miner.emaxsum)

;;; Maximum sum
;;; https://gist.github.com/ericnormand/3057e41cf4eddd1297afb7ddfa94d234

;; Write a function that returns the maximum sum of a contiguous subsequence of integers in
;; a vector.  They say you can do it in linear time and constant space. Just make sure it
;; works with large vectors.


(defn max-sum [vvv]
  (let [zadd (fn [a b] (max (+ a b) 0))]
    (reduce max (reductions zadd 0 vvv))))


;;; @sw got it right first
(defn sw-max-sum [xs]
  (reduce max (reductions #(max 0 (+ %1 %2)) 0 xs)))


;;; commenter points to Kadane's algorithm:
;;; https://en.wikipedia.org/wiki/Maximum_subarray_problem#Kadane's_algorithm

;;; SEM notes to myself.  Went through a bunch of ideas about pre-combining runs of negs and
;;; pos but not worth all that manipulation.  The idea of grouping was kind of correct, but
;;; the mechanics favor brute force additions.  At one point, I thought it had to look both
;;; ways but that was a mistake.

;; arg is required to be vector so no need to vectorize

;; slightly faster with type hints
;; leading drops not worth it


(def v1000 [-39 -21 -16 34 41 -1 6 32 31 35 18 33 29 -10 28 21 -27 14 -7 -17 6 -10 -49 -30
            -47 21 -43 -33 -27 23 -26 2 -48 -46 -41 -12 -5 26 -29 -5 39 -44 11 18 27 -31 20
            7 47 47 -1 21 -17 20 16 46 44 38 14 -46 -36 -1 35 21 17 -28 -35 7 -29 46 33 32
            35 -45 -1 45 -30 5 3 -16 34 -43 29 34 -13 4 25 22 -47 7 49 1 -49 -33 -18 27 15
            10 -1 29 12 8 31 29 23 -43 -13 2 -2 -24 35 8 22 9 4 -47 -5 -46 14 11 -49 -30 -29
            -34 -43 48 -32 -7 -24 -5 12 -43 -9 -5 -30 -44 -24 -20 43 11 -44 -12 36 -43 45 36
            17 -35 -12 1 31 -4 4 -47 -29 -40 36 -48 -45 21 -49 -7 -45 6 -38 40 13 -7 -6 -25
            48 27 49 -47 -14 7 42 45 -9 -22 16 22 37 -36 40 29 -15 -41 44 -32 24 35 -10 30
            38 -32 45 -22 3 -28 -14 6 -38 -42 33 -9 -47 34 49 41 48 -4 -9 -1 41 -40 48 2 -27
            45 39 -21 15 -4 42 34 24 12 24 10 4 -50 -26 46 25 22 22 -45 39 44 10 -10 17 29 4
            3 -49 -21 -4 19 33 -34 -1 -3 -13 43 -19 -2 41 -2 -21 3 -34 9 31 0 -26 20 25 -15
            -42 24 15 -11 -37 23 -38 0 -40 -30 -27 5 31 23 -22 33 7 -43 -43 -21 24 21 40 8
            22 -24 31 -47 -7 33 4 -18 29 24 -32 -20 33 16 -38 -11 47 -26 -38 25 -41 -15 -31
            -20 -44 -46 -29 10 -20 -17 -35 7 7 -32 40 48 -18 -50 -38 13 8 -45 -32 -27 38 20
            -24 -49 10 -21 33 -29 23 25 11 33 -27 0 -24 43 -24 -18 45 -20 -4 -31 13 -23 -48
            19 -48 34 32 22 29 7 -44 28 -31 41 21 9 -36 -29 47 -18 5 24 13 -47 5 20 -45 38
            32 7 -23 20 35 13 49 19 10 45 -5 -24 15 22 -43 44 43 -9 5 34 -15 27 11 -50 -35
            -11 35 24 19 -40 -22 29 -6 -20 40 48 -27 -48 0 -39 20 -36 48 -11 3 -29 -43 39 3
            42 49 -3 45 -28 5 -8 -12 -46 11 -26 -16 -20 -16 -35 -8 -44 0 9 -41 -12 -18 17 -4
            29 21 -39 -26 47 -7 35 44 8 29 -16 11 -37 16 -27 -43 -48 -32 25 -27 36 -14 -21
            -12 22 35 -39 -1 15 19 -36 -9 28 -24 49 24 36 29 7 -26 -50 -32 0 -15 -15 38 26
            -43 -40 46 3 -9 -8 -20 49 -30 -7 -43 -41 2 45 5 -37 37 45 -23 15 3 -1 -17 20 -50
            -8 16 -17 -50 -9 -1 40 29 -33 -16 3 24 7 -8 39 7 34 4 -18 41 -37 46 -42 0 42 -7
            8 -20 -22 -26 -20 49 48 -46 36 36 -37 28 -29 38 -43 36 -20 18 -40 34 -31 30 1
            -41 -26 30 -6 38 0 -31 -43 21 -31 27 21 9 18 -44 -15 27 -12 22 -4 34 -20 -26 29
            4 -34 -21 -14 1 8 33 15 -8 20 38 5 -46 2 -34 33 34 -46 39 42 -25 -3 -17 2 -3 -20
            47 10 16 -5 -27 33 1 7 -2 47 -15 -6 5 -46 29 42 15 30 -24 5 -22 -31 -43 -7 -33
            -1 -25 42 8 40 27 -30 -39 -12 -7 1 -36 35 39 32 1 20 11 -22 -19 -20 44 19 -49 14
            31 -12 20 -5 -34 -38 -18 -4 -24 -4 44 12 -17 -29 -9 33 -29 -50 45 5 40 44 28 21
            6 -10 25 -25 -21 38 -40 -10 -31 -13 20 -25 13 -9 -34 17 -13 2 29 -34 -34 9 39
            -42 18 -2 -23 34 -30 33 31 42 -12 39 29 20 -16 -39 -15 -22 -44 46 30 35 -28 -7
            24 -22 -38 10 43 -37 -31 9 -3 -6 -22 -28 9 -30 8 18 1 18 -22 24 45 -15 -12 27 30
            6 -3 -7 -42 14 18 11 21 10 27 7 -50 -12 -45 3 -3 28 3 -48 26 -16 23 15 20 23 46
            -17 39 -48 -10 -30 20 -40 -21 41 32 36 22 4 48 -41 33 -25 -37 -19 -37 -36 -25
            -37 10 -12 20 -40 -4 24 34 -16 -30 -38 -29 -39 -21 -33 -22 -43 48 11 -16 21 -20
            42 8 18 34 -30 36 -3 43 -31 -30 7 -30 36 -10 5 48 46 -44 27 28 7 -5 -25 -25 14
            39 44 -2 -16 -41 2 -42 44 30 -15 -12 31 -18 -13 3 -23 43 -24 9 19 -40 -6 9 26 -8
            39 14 -20 -9 -16 -2 14 -25 22 -2 -28 -49 -16 -33 21 10 38 -17 11 14 5 -2 45 20
            41 48 17 35 30 -2 15 -9 -35 -22 11 21 19 -37 10 -43 -8 37 26 -35 16 -34 -37 -30
            40 -4 29 10 45 30 -40 -39 -20 -40 -21 -35 33 42 -36 22 22 -13 -40 2 -2 -6 22 36
            33 -9 -46 -1 -6 -19 35 48 -26 -39 -42 5 -5 -17 -4 -42 -30 -48 -39 -50 -43 7 37
            -28 44 22 11 17 -1 -29 26])
  
(defn smoke-sum [max-sum]
  ;; (sum of empty seq is 0)
  (assert (= (max-sum []) 0))
  (assert (= (max-sum [1])  1))
  (assert (= (max-sum [1 10]) 11))
  ;; because you can choose the empty subsequence, which has sum 0
  (assert (= (max-sum [-1]) 0))
  ;;  sum of [2 4 -2 7]
  (assert (= (max-sum [3 -6 2 4 -2 7 -9]) 11))
  (assert (= (max-sum [50 -100 25 30 -100]) 55))
  (assert (= (max-sum [10 20 -1 5 -100 31 -100]) 34))
  (assert (= (max-sum [0 -1 -2 0 -3 4 0 5 -10 3 1 1]) 9))
  (assert (= (max-sum v1000) 713))
  true)









;;; ----------------------------------------------------------------------
;;; ohter junk  and a lot of mistakes



(defn max-indices [ikey coll]
  (when (seq coll)
    (loop [i 1
           res [0]
           mx (ikey (first coll))
           cs (rest coll)]
      (if (empty? cs)
        res
        (let [k (ikey (first cs))]
          (cond (= k mx) (recur (inc i) (conj res i) mx (rest cs))
                (> k mx) (recur (inc i) [i] k (rest cs))
                :else (recur (inc i) res mx (rest cs))))))))

;;; BUG center assumption is false -- must consider any positive starting point



(defn extend-center [vsum center]
  (let [pre (apply max (reductions + 0 (rseq (subvec vsum 0 center))))
        post (apply max (reductions + 0 (subvec vsum (inc center))))]
    ;;(println "vsum" vsum center pre post)
    (+ pre post (vsum center))))

(defn max-sum-WORKS [coll]
  (let [vsum (into [] (comp (partition-by pos?)
                            (map #(reduce + %)))
                   coll)
        vcnt (count vsum)]
    (case vcnt
      0 0
      1 (max (vsum 0) 0)
      (reduce max 0 (map #(extend-center vsum %)
                         (range (if (pos? (first vsum)) 0 1) vcnt 2))))))

(defn max-sum22 [coll]
  (let [vsum (into [] (comp (partition-by pos?)
                            (map #(reduce + %)))
                   coll)
        vcnt (count vsum)]
    (case vcnt
      0 0
      1 (max (vsum 0) 0)
      (reduce max 0 (map #(extend-center vsum %)
                         (range (if (pos? (first vsum)) 0 1) vcnt 2))))))

;;; about same speed as zsum -- maybe better with transducers!
(defn xsum1 [coll]
  (let [vsum (into [] (comp (partition-by neg?)
                            (map #(reduce + %)))
                   coll)
        vcnt (count vsum)
        extend (fn [center]
                 (let [pre (apply max (reductions + 0 (rseq (subvec vsum 0 center))))
                       post (apply max (reductions + 0 (subvec vsum (inc center))))]
                   (+ pre post (vsum center))))]
    (transduce (map extend)
               max
               0
               (range (if (or (zero? vcnt) (neg? (vsum 0))) 1 0) vcnt 2))))



;; drop-while is apparently slower
(defn xsum3 [coll]
  (let [vsum (into [] (comp (remove zero?)
                            (drop-while neg?)
                            (partition-by neg?)
                            (map #(reduce + %)))
                   coll)
        extend (fn [center]
                 (let [pre (apply max (reductions + 0 (rseq (subvec vsum 0 center))))
                       post (apply max (reductions + 0 (subvec vsum (inc center))))]
                   (+ pre post (vsum center))))]
    (transduce (map extend)
               max
               0
               (range 0 (count vsum) 2))))



;; best ysum but still slow
(defn ysum2 [coll]
  (let [vvv (into [] (comp (remove zero?) (drop-while neg?)) coll)]
    (if (zero? (count vvv))
      0
      (let [vsum (reduce (fn [pv i]
                             (let [p (peek pv)]
                               (if (neg? p)
                                 (if (neg? i) (conj (pop pv) (+ p i)) (conj pv i))
                                 (if (pos? i) (conj (pop pv) (+ p i)) (conj pv i)))))
                         [(nth vvv 0)]
                         (subvec vvv 1))
            extend (fn [center]
                     (let [pre (apply max (reductions + 0 (rseq (subvec vsum 0 center))))
                           post (apply max (reductions + 0 (subvec vsum (inc center))))]
                       (+ pre post (vsum center))))]
        (transduce (map extend)
                   max
                   0
                   (range 0 (count vsum) 2))))))





(defn abs>= [a b]
  (let [a (if (neg? a) (- a) a)
        b (if (neg? b) (- b) b)]
    (>= a b)))

(defn zadd [a b]
  (let [ab (+ a b)]
    (if (neg? ab)
      0
      ab)))

;;; reductions zadd for the win.  The pre-grouping is a red-herring.  Not worth it.
(defn ysum23 [coll]
  (let [vvv (into [] (comp (remove zero?) (drop-while neg?)) coll)]
    (if (zero? (count vvv))
      0
      (let [vsum (reduce (fn [pv i]
                             (let [p (peek pv)]
                               (if (neg? p)
                                 (if (neg? i) (conj (pop pv) (+ p i)) (conj pv i))
                                 (if (pos? i) (conj (pop pv) (+ p i)) (conj pv i)))))
                         [(nth vvv 0)]
                         (subvec vvv 1))]
        (reduce max (concat (reductions zadd 0 vsum)
                            (reductions zadd 0 (rseq vsum))))))))


;;; just testing,  tests too many cases
(defn pcombos [cnt]
  (for [start (range 0 cnt 2)
        width (range 1 (inc cnt) 2)
        :let [end (+ start width)]
        :when (<= end cnt)]
    [start end]))
