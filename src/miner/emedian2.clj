(ns miner.emedian2)

;; Not an Eric challenge but similar idea:
;; "Using Scheme to Find the Median of Two Sorted Integer Lists"
;;
;; https://www.erichgrunewald.com/posts/using-scheme-to-find-the-median-of-two-sorted-integer-lists/
;;
;; The point is to avoid doing the merge and sort O(m+n).  Use a binary search approach to
;; get an O(log(m+n)) algorithm.  Surely this matters only for very large collections.  And
;; my guess is that constant factors matter more.  Let's find out.
;;
;; Median definition:  the central value if the list has an odd-numbered length, or the mean
;; of the two central values otherwise.  (SEM: not my favorite but that's the problem
;; definition.)


;; First, I want to benchmark the naive merge and sort approach.  It's useful for testing in
;; any case.

;; Let's assume a and b are pre-sorted vectors of integers.
;; BTW, we're not going to worry about overflows.
;; Actually, ignores the guarantee that a and b were sorted

(defn brute-mergesort [a b]
  (vec (sort (into a b))))

;; really should be called monotonically-non-descending?
(defn ascending? [a]
  (reduce (fn [r x] (if (<= r x) x (reduced false))) (nth a 0 true) a))

(defn brute-med2 [a b]
  ;;{:pre [(vector? a) (vector? b) (ascending? a) (ascending? b)]}
  (let [ab (brute-mergesort a b)
        cnt (count ab)
        mid (quot cnt 2)]
    (if (odd? cnt)
      (ab mid)
      (/ (+ (ab mid) (ab (dec mid))) 2))))


(defn mergesort [a b]
  (let [alen (count a)
        blen (count b)]
    (loop [c [] i 0 j 0]
      (cond (>= i alen) (into c (subvec b j))
            (>= j blen) (into c (subvec a i))
            (<= (a i) (b j)) (recur (conj c (a i)) (inc i) j)
            :else (recur (conj c (b j)) i (inc j))))))

;; not worth it
(defn tmergesort [a b]
  (let [alen (count a)
        blen (count b)
        into! #(reduce conj! % %2)]
    (persistent!
     (loop [c (transient []) i 0 j 0]
       (cond (>= i alen) (into! c (subvec b j))
             (>= j blen) (into! c (subvec a i))
             (<= (a i) (b j)) (recur (conj! c (a i)) (inc i) j)
             :else (recur (conj! c (b j)) i (inc j)))))))


(defn mmed2 [a b]
  (let [ab (mergesort a b)
        cnt (count ab)
        mid (quot cnt 2)]
    (if (odd? cnt)
      (ab mid)
      (/ (+ (ab mid) (ab (dec mid))) 2))))

(defn tmed2 [a b]
  (let [ab (tmergesort a b)
        cnt (count ab)
        mid (quot cnt 2)]
    (if (odd? cnt)
      (ab mid)
      (/ (+ (ab mid) (ab (dec mid))) 2))))



(def v1k (vec (range 1000)))
(def v3k (vec (range 0 3003 3)))


(defn smoke-med [med]
  (assert (= (med [1 2 3] [4 5 6 7]) 4))
  (assert (= (med v1k v3k) 750))
  true)


;; This should be faster if we're smarter about just looking for the median without having to
;; mergesort.  That's the point of the original article.

(defn bmed2 [a b]
  (let [total (+ (count a) (count b))
    (loop [needed (quot (inc total) 2)
           i (quot (count a) 2)
           j (quot (count b) 2)]
        (cond (<= (a i) (b (inc j)) [i j]
              (<= (b midb) (a (inc mida))) (recur (- needed (- midb b0)) a0 an midb bn)
              (<= (a mida) (b midb)) (recur needed a0 aN b
