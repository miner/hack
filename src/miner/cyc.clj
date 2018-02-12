(ns miner.cyc)

;; Advent of Code 2017
;; day 1

;; Sum of digits that match next digit.  Last wraps around to first.

;; Don't use this.  Better to use nth with default.
(defn OBSOLETE-get-wrap [v i]
  (if (= i (count v))
    (v 0)
    (v i)))

;; use default, but better to inline
;; really better to not need it by checking (dec (count dv))
(defn nth-wrap [v i]
  (nth v i (v 0)))

(defn str->dv [str-or-n]
  (assert (Long/parseLong (str str-or-n)))
  (mapv (fn [c] (- (long c) (long \0))) (str str-or-n)))

;; fastest now, avoid intermediate seq, just vector access
;; avoid default access by special casing the last index
;; pos? test to avoid bug with single digit case
;; could be faster without the :pre or turn off *assert*
(defn day1 [s]
  {:pre [(Long/parseLong (str s))]}
  (let [dv (mapv (fn [c] (- (long c) (long \0))) (str s))
        ilast (dec (count dv))]
    (reduce (fn [sum i] (if (= (dv i) (dv (inc i))) (+ sum (dv i)) sum))
            (if (and (pos? ilast) (= (dv ilast) (dv 0))) (dv 0) 0)
            (range ilast))))



;; SEM:  many attempts below are buggy with single digit, but that's open to interpretion
;; added (pos? ilast) test to fix, or (count dv) > 1



;; SEM -- added test for single digit, empty string still considered user error
;;; quick test
(defn smoke
  ([] (smoke day1))
  ([f]
   (assert (= (f 3) 0))
   (assert (= (f "1122") 3))
   (assert (= (f "1111") 4))
   (assert (= (f "1234") 0))
   (assert (= (f "91212129") 9))
   (assert (= (f 91212129) 9))
   true))



;;; other attempts

(defn str->digits [str-or-n]
  (assert (Long/parseLong (str str-or-n)))
  (map (fn [c] (- (long c) (long \0))) (str str-or-n)))

  
(defn next-pairs [coll]
  (let [v (vec coll)]
    (partition 2 1 (conj v (v 0)))))

(defn sum-matching [coll]
  (reduce + (map (fn [[n m]] (if (= n m) n 0)) (next-pairs coll))))



(defn day1slow [s]
  (sum-matching (str->digits s)))


(defn day1gbad [s]
  (let [ds (str->digits s)]
    (reduce + (map (fn [a b] (if (= a b) a 0)) ds (rest (cycle ds))))))

(defn day1gsoso [s]
  (let [ds (str->digits s)]
    (if (next ds)
      (reduce + (if (= (first ds) (last ds)) (first ds) 0)
              (map (fn [a b] (if (= a b) a 0)) (rest ds) ds))
      0)))

;; pretty good
(defn day1gg [s]
  (let [dv (str->dv s)
        ds (seq dv)]
    (if (next ds)
      (reduce + (if (= (first ds) (peek dv)) (first ds) 0)
              (map (fn [a b] (if (= a b) a 0)) (rest ds) ds))
      0)))

(defn day1g2 [s]
  (let [dv (str->dv s)
        ds (seq dv)]
    (reduce + (if (and (next ds) (= (first ds) (peek dv))) (first ds) 0)
              (map (fn [a b] (if (= a b) a 0)) (rest ds) ds))))


(defn day1g3 [s]
  (let [dv (str->dv s)
        ds (seq dv)]
    (reduce + (if (and (next ds) (= (first dv) (peek dv))) (first dv) 0)
            (map (fn [a b] (if (= a b) a 0)) (rest ds) ds))))


(defn day1good [s]
  (let [ds (str->digits s)]
    (if (next ds)
      (reduce + (map (fn [a b] (if (= a b) a 0)) ds (rest (cycle ds))))
      0)))

(defn day1h [s]
  (let [dv (str->dv s)
        wrap (when (> (count dv) 1) (conj dv (first dv)))]
    (reduce + (map (fn [a b] (if (= a b) a 0)) wrap (rest wrap)))))



(defn day1i [s]
  (let [ds (str->digits (str s))
        wrap (when (next ds) (conj ds (last ds)))]
    (reduce + (map (fn [a b] (if (= a b) a 0)) ds wrap))))

;; second fastest
(defn day1a [s]
  (let [dv (str->dv s)]
    (reduce + (map-indexed (fn [i x] (if (= x (nth dv (inc i) (dv 0))) x 0)) dv))))


;; not faster, probably because transducer set up time dominates processing
(defn day1s [s]
  (let [ds (str->digits s)]
    (reduce + (sequence (map (fn [a b] (if (= a b) a 0))) ds (rest (cycle ds))))))

(defn day1t [s]
  (let [ds (str->digits s)]
    (transduce (comp (partition-all 2)  (map (fn [[a b]] (if (= a b) a 0))))
               +
               0
               (interleave ds (rest (cycle ds))))))



(defn day1b [s]
  (let [dv (str->dv s)]
    (reduce + 0 (map (fn [a b] (if (= a b) a 0)) dv (subvec (conj dv (first dv)) 1)))))

(defn day1c [s]
  (let [dv (str->dv s)]
    (reduce + 0 (map (fn [a b] (if (= a b) a 0)) dv (rest (conj dv (first dv)))))))

(defn day1d [s]
  (let [dv (str->dv s)]
    (reduce + 0 (map (fn [i] (if (= (dv i) (nth dv (inc i) (dv 0))) (dv i) 0))
                     (range (count dv))))))


(comment
  (use 'criterium.core)
  
  (doseq [fname (apropos "day1")]
    (println)
    (println fname)
    (let [f (resolve fname)]
      (if  (try (smoke f) (catch AssertionError _ false))
        (quick-bench (smoke f))
        (println fname "failed"))))

  ;; end comment
)

  
