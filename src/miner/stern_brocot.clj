;; https://www.rosettacode.org/wiki/Stern-Brocot_sequence#Clojure

;; Similar to a Fibonacci sequence but with a different generating rule.
;; Start with [1,1].
;; Consider second element, sum it and predecessor, append result.
;; [1 1 2]
;; Then, append the "considered" member.
;; [1 1 2 1]
;; Consider the next member (third element) and loop, appending sum and self each time.
;; [1 1 2 1 3 2]
;; continue with fourth, etc -- always append two elements according to rule.
;; First 15 elements are: [1, 1, 2, 1, 3, 2, 3, 1, 4, 3, 5, 2, 5, 3, 4]

(ns miner.stern-brocot)

;; each step adds two items
(defn sb-step [v]
  (let [i (quot (count v) 2)]
    (conj v (+ (v (dec i)) (v i)) (v i))))


;; A lazy, infinite sequence -- `take` what you want.
;; Inspired by Christophe Grand's Fibonacci solution on the Clojure mailing list.
;; Var caches largest results ever used, but never shrinks.
(def all-sbs (sequence (map peek) (iterate sb-step [1 1])))


;; zero-based
(defn first-appearance [n]
  (first (keep-indexed (fn [i x] (when (= x n) i)) all-sbs)))

;; inlined abs; rem is slightly faster than mod, and the same result for positive values
(defn gcd [a b]
  (loop [a (if (neg? a) (- a) a)
         b (if (neg? b) (- b) b)]
    (if (zero? b)
      a
      (recur b (rem a b)))))

(defn check-pairwise-gcd [cnt]
  (let [sbs (take (inc cnt) all-sbs)]
    (every? #(= 1 %) (map gcd sbs (rest sbs)))))

;; one-based index required by problem statement
(defn report-sb []
  (println "First 15 Stern-Brocot members:" (take 15 all-sbs))
  (println "First appearance of N at 1-based index:")
  (doseq [n [1 2 3 4 5 6 7 8 9 10 100]]
    (println " first" n "at" (inc (first-appearance n))))
  (println "Check pairwise GCDs = 1 ..." (check-pairwise-gcd 1000))
  true)








;;; Solutions from RosettaCode.org

;; bad recursion, and slow

(defn nth-stern-brocot [n] 
  (if (< n 2) 
    n
    (let [h (quot n 2) h1 (inc h) hth (nth-stern-brocot h)]
      (if (zero? (mod n 2)) hth (+ hth (nth-stern-brocot h1))))))
 
;; return a lazy version of the entire Stern-Brocot sequence 
(defn stern-brocot
  ([] (stern-brocot 1))
  ([n] (cons (nth-stern-brocot n) (lazy-seq (stern-brocot (inc n))))))

;; Second example
;; bad concat, should stay in vector -- see next section
(defn stern-brocat-next [p]
  " p is the block of the sequence we are using to compute the next block
    This routine computes the next block "
  (into [] (concat (rest p) [(+ (first p) (second p))] [(second p)])))

;; SEM: note misspelling
(defn seq-stern-brocat
  ([] (seq-stern-brocat [1 1]))
  ([p] (lazy-seq (cons (first p)
                       (seq-stern-brocat (stern-brocat-next p))))))

;;;;;;;;;;;;;;;;;;

;; SEM rewriting...

;; much faster to stay with vector p, but not faster than lazy-sbs
(defn sb-next [p]
  " p is the block of the sequence we are using to compute the next block
    This routine computes the next block "
  (conj (subvec p 1)  (+ (nth p 0) (nth p 1)) (nth p 1)))

(defn seq-sb
  ([] (seq-sb [1 1]))
  ([p] (lazy-seq (cons (peek p) (seq-sb (sb-next p))))))




;; Queue approach is faster than subvec, about same as lazy-sbs
(defn queue
  ([] clojure.lang.PersistentQueue/EMPTY)
  ([coll] (reduce conj clojure.lang.PersistentQueue/EMPTY coll)))

(defn qsb-next [q]
  (let [q2 (pop q)
        x1 (peek q2)]
    (conj q2 (+ (peek q) x1) x1)))

(defn qsbs
  ([] (qsbs (queue [1 1])))
  ([q] (lazy-seq (cons (peek q) (qsbs (qsb-next q))))))



;;;;;;;;;;;;;;;;;;
(defn sem-sbs [] all-sbs)


(defn smoke [f]
  (assert (= (take 100 (f)) (take 100 all-sbs)))
  true)



(defn smoke-test []
  (assert (= [1, 1, 2, 1, 3, 2, 3, 1, 4, 3, 5, 2, 5, 3, 4]
             (take 15 all-sbs)))
  ;; zero-based first-appearance
  (assert (= (map first-appearance [1 2 3 4 5 6 7 8 9 10 100])
             [0 2 4 8 10 32 18 20 34 38 1178]))
  (assert (check-pairwise-gcd 1000))
  true)



(defn lazy-sbs []
  (letfn [(lzs [v] (lazy-seq (cons (peek v) (lzs (sb-step v)))))]
     (lzs [1 1])))



(defn lz-sbs []
  (let [xxx (fn lzs [v] (lazy-seq (cons (peek v) (lzs (sb-step v)))))]
     (xxx [1 1])))




;; See also:   https://oeis.org/A002487
;; Stern's diatomic series (or Stern-Brocot sequence): a(0) = 0, a(1) = 1;
;;   for n > 0: a(2*n) = a(n), a(2*n+1) = a(n) + a(n+1). 
;;
;; Note: the OEIS starts with 0 so it's slightly different than the Rosetta problem.  IMHO,
;; the OEIS version has a clearer mathematical definition, but the code isn't as nice.

(def all-oeis (sequence (map peek) (cons [0] (iterate sb-step [1 1]))))

;; Or re-working from scratch...

;; OEIS version starting with [0 1]
;; need to extend v first so that next index is accessible for initial blocks
(defn oeis-step [v]
  (let [i (quot (count v) 2)
        v (conj v (v i))]
    (conj v (+ (v i) (v (inc i))))))

;; repeated item is now second to last so pop then peek, not quite as clean as Rosetta
(def oeis (sequence (map (comp peek pop)) (iterate oeis-step [0 1])))

;; based on oeis [0 1] start, but if you ignore element 0, it works for Rosetta 1-based indexing
(defn nth-sb-oeis
  ([n] (nth-sb-oeis 0 (list n)))
  ([sum xs]
   (let [x (first xs)]
     (cond (nil? x) sum
           (< x 2) (recur (+ sum x) (rest xs))
           (even? x) (recur sum (cons (quot x 2) (rest xs)))
           :else (let [h (quot x 2)] (recur sum (list* h (inc h) (rest xs))))))))


;; You might want to make a caching version of nth-sb for better performance.   :-)  Try `memoize`

(defn elt-sb [n]
  (if (zero? n)
    0
    (first (drop (dec n) all-sbs))))


;; If you want to start with 1 and be 0-indexed, which is natural for Clojure, you need to
;; adjust the logic a bit.


(defn nth-sb
  ([n] (nth-sb 0 (list n)))
  ([sum xs]
   (let [x (first xs)]
     (cond (nil? x) sum
           (< x 2) (recur (inc sum) (rest xs))
           (odd? x) (recur sum (cons (quot x 2) (rest xs)))
           :else (let [h (quot x 2)] (recur sum (list* (dec h) h (rest xs))))))))

;; INCOMPLETE -- want to memoize the base case, but use the memo in the recursive code.

(def memsb (memoize #(nth-sb 0 (list %))))




;; Hack alert! -- this is slightly faster than normal memoize, but it's ugly.  Really better
;; and faster to use the all-sbs approach.

;; inlining memoize and nth-sb
(let [mem (atom {})]
  (defn msb
    ([] @mem)
    ([n] (msb n 0 (list n)))
    ([n sum xs]
     (let [x (first xs)]
       (if (nil? x)
         (do (swap! mem assoc n sum) sum)
         (if-let [cached (get @mem n)]
           cached
           (cond (< x 2) (recur n (inc sum) (rest xs))
                 (odd? x) (recur n sum (cons (quot x 2) (rest xs)))
                 :else (let [h (quot x 2)] (recur n sum (list* (dec h) h (rest xs)))))))))))




;; thinking about trying to save the vector state and use it directly
(def all-vec-sbs (iterate sb-step [1 1]))

(defn vec-sbs [cnt]
  (last (take (quot (inc cnt) 2) all-vec-sbs)))

(def all-sbs2 (sequence (map peek) all-vec-sbs))


;; slightly more complicated to step one item at a time, not any faster, so don't bother
(defn sb-step1 [v]
  (let [cnt (count v)
        i (quot cnt 2)]
    (if (odd? cnt)
      (conj v (v i))
      (conj v (+ (v (dec i)) (v i))))))

(def all-sbs1 (sequence (comp (take-nth 2) (map peek)) (iterate sb-step1 [1 1])))
