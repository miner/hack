(ns eric.intp)


;;; https://gist.github.com/ericnormand/a3489d47e163d84650a7c2f5ff32ecd6

;;; Primes in a number

;;; Another contrived math problem. This one I think is actually pretty hard. It's got
;;; detecting primes, string manipulation, and combinations.  Your task is to write a
;;; function that takes an integer and finds all primes that are substrings of the decimal
;;; digits of that integer.  Return the primes in ascending order.  If a prime appears more
;;; than once, it should be in the returned sequence that many times.


(defn prime? [n]
  (if (even? n)
    (= n 2)
    (and (> n 1)
         (let [sqrt (Math/sqrt n)]
           (loop [candidate 3]
             (cond (> candidate sqrt) true
                   (zero? (rem n candidate)) false
                   :else (recur (inc (inc candidate)))))))))


(defn find-primes [n]
  (let [digs (into () (comp (take-while pos?) (map #(rem % 10))) (iterate #(quot % 10) n))
        parts (reduce (fn [ps w] (into ps (partition w 1 digs)))
                      []
                      (range 1 (inc (count digs))))
        cands (map #(reduce (fn [r i] (+ (* 10 r) i)) 0 %) parts)]
    (sort (filter prime? cands))))




(defn smoke-fp [find-primes]
  (assert (= (find-primes 2)  [2]))
  (assert (= (find-primes 22) [2 2]))
  (assert (= (find-primes 717) [7 7 17 71]))
  (assert (= (find-primes 23717) [2 3 7 7 17 23 37 71 2371]))
  (assert (= (find-primes 1)  []))
  (assert (= (find-primes 44) []))
  true)







(defn infinite-primes []
  (let [sieve (fn sieve [s]
                ;; first item is always a known prime, rest are candidates
                (lazy-seq
                 (cons (first s)
                       (sieve (remove #(zero? (rem % (first s))) (rest s))))))]
    (sieve (cons 2 (iterate #(+ % 2) 3)))))

;; doesn't cache primes so kind of expensive
;; could just keep one active sequence of largest prime we have ever needed
;; need to keep largest to decide when to expand
;; sorted-set is maybe slow and you only need to know max so maybe map or meta

;;; too slow to recalc all primes < N every time.
(defn find-primes1 [n]
  (let [nprimes (set (take-while #(<= % n) (infinite-primes)))
        dv (into nil (comp (take-while pos?) (map #(mod % 10))) (iterate #(quot % 10) n))
        parts (reduce (fn [ps w] (into ps (partition w 1 dv)))
                   []
                   (range 1 (inc (count dv))))
        cands (map #(reduce (fn [r i] (+ (* 10 r) i)) 0 %) parts)]
    (sort (filter nprimes cands))))



(defn bar-primes
  "Returns sequence of primes less than N"
  [^long n]
  (if (<= n 2)
    ()
    (let [n (if (odd? n) (dec n) n)
          nsqrt (long (Math/sqrt n))]
      (loop [cmps (boolean-array n) i 3]
        (cond
         (> i nsqrt) (loop [b (dec n) ps ()]
                        (if (< b 3)
                          (conj ps 2)
                          (recur (- b 2) (if (aget cmps b) ps (conj ps b)))))
         (aget cmps i) (recur cmps (+ i 2))
         :else (recur (let [step (* 2 i)]
                        (loop [cmps cmps j (* i i)]
                          (if (< j n)
                            (recur (do (aset cmps j true) cmps) (+ j step))
                            cmps)))
                      (+ i 2)))))))



(defn findp [n]
  (let [nprimes (set (bar-primes (inc n)))
        dv (into nil (comp (take-while pos?) (map #(mod % 10))) (iterate #(quot % 10) n))
        parts (reduce (fn [ps w] (into ps (partition w 1 dv)))
                   []
                   (range 1 (inc (count dv))))
        cands (map #(reduce (fn [r i] (+ (* 10 r) i)) 0 %) parts)]
    (sort (filter nprimes cands))))



;;; slower than bar-primes
(defn bit-primes
  "Returns sequence of primes less than N"
  [^long n]
  (if (<= n 2)
    ()
    (let [n (if (odd? n) (dec n) n)
          nsqrt (long (Math/sqrt n))]
      (loop [cmps (java.util.BitSet. n) i 3]
        (cond
         (> i nsqrt) (loop [b (dec n) ps ()]
                        (if (< b 3)
                          (conj ps 2)
                          (recur (- b 2) (if (.get ^java.util.BitSet cmps (int b))
                                           ps
                                           (conj ps b)))))
         (.get ^java.util.BitSet cmps (int i)) (recur cmps (+ i 2))
         :else (recur (let [step (* 2 i)]
                        (loop [cmps cmps j (* i i)]
                          (if (< j n)
                            (recur (do (.set ^java.util.BitSet cmps (int j)) cmps) (+ j step))
                            cmps)))
                      (+ i 2)))))))

;; but 2x slower!
(defn bit-primes2
  "Returns sequence of primes less than N"
  [^long n]
  (let [bset (fn [cmps i] (doto ^java.util.BitSet cmps (.set (int i))))
        bget (fn [cmps i] (.get ^java.util.BitSet cmps (int i)))]
    (if (<= n 2)
      ()
      (let [n (if (odd? n) (dec n) n)
            nsqrt (long (Math/sqrt n))]
        (loop [cmps (java.util.BitSet. n) i 3]
          (cond
           (> i nsqrt) (loop [b (dec n) ps ()]
                         (if (< b 3)
                           (conj ps 2)
                           (recur (- b 2) (if (bget cmps b) ps (conj ps b)))))
           (bget cmps i) (recur cmps (+ i 2))
           :else (recur (let [step (* 2 i)]
                          (loop [cmps cmps j (* i i)]
                            (if (< j n)
                              (recur (bset cmps j) (+ j step))
                              cmps)))
                        (+ i 2))))))))

;; idea is to keep a cache of bitset than can be augmented on demand
;; access must check for 2 or (d2 p)  only "odds" are in bit set so 2i+1 = p


(defn bitset-not-primes
  [^long n]
  (let [d2 (fn [i] (bit-shift-right i 1))
        bset (fn [cmps i] (doto ^java.util.BitSet cmps (.set (int (d2 i)))))
        bget (fn [cmps i] (.get ^java.util.BitSet cmps (int (d2 i))))]
    (if (<= n 2)
      ()
      (let [n (if (odd? n) (dec n) n)
            nsqrt (long (Math/sqrt n))]
        (loop [cmps (doto (java.util.BitSet. (d2 n)) (.set 0)) i 3]
          (cond
           (> i nsqrt) cmps
           (bget cmps i) (recur cmps (+ i 2))
           :else (recur (let [step (* 2 i)]
                          (loop [cmps cmps j (* i i)]
                            (if (< j n)
                              (recur (bset cmps j) (+ j step))
                              cmps)))
                        (+ i 2))))))))


;; should extend cmps from existing, not just recalc
;; have to do a lot of recalc to do the extensions
;; all the old primes must cancel composites in the extended range

;; check bitset .size /2 to see if p has been cached
(let [cache (atom (bitset-not-primes 1000))]
  (defn bprime? [n]
    (cond (= n 2) true
          (even? n) false
          :else
              (loop [cmpcache @cache]
                (if (< n (* 2 (.size ^java.util.BitSet cmpcache)))
                  (not (.get ^java.util.BitSet cmpcache (int (bit-shift-right n 1))))
                  (recur (reset! cache (bitset-not-primes n))))))))

