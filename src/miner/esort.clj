(ns miner.esort)

;;; https://gist.github.com/ericnormand/48b0a933294639bb27902eda062772e2

;;; In this task, we are sorting a heterogeneous list of elements. The elements are either
;;; single numbers or sequences of numbers. We should sort them by numeric content, like
;;; this:  we sort sequences by their first element, then by second element, then by third,
;;; etc; we sort numbers and sequences together as if numbers were sequences of length 1;
;;; but numbers sort before sequences when the number is the same as the first element.


;;; Original submission had bug -- infinite loop on multiple equal sequences.  Fixed with
;;; empty? tests.

;; actually faster to use number? vs not coll?
(defn sort-by-content [xs]
  (let [icompare (fn [a b]
                   (let [a1? (number? a)
                         b1? (number? b)]
                     (cond (and a1? b1?) (compare a b)
                           a1? (if (<= a (first b)) -1 1)
                           b1? (if (< (first a) b) -1 1)
                           :else (loop [a (seq a) b (seq b)]
                                   (let [c (compare (first a) (first b))]
                                     (cond (not (zero? c)) c
                                           (and (empty? a) (empty? b)) 0
                                           :else (recur (rest a) (rest b))))))))]
    (sort icompare xs)))







;;; assumes vec
(defn scp [xs]
  (let [icompare (fn [a b]
                   (let [a1? (number? a)
                         b1? (number? b)]
                     (cond (and a1? b1?) (compare a b)
                           a1? (if (<= a (first b)) -1 1)
                           b1? (if (< (first a) b) -1 1)
                           :else (let [acnt (count a)
                                       bcnt (count b)
                                       pad (max acnt bcnt)
                                       a (if (= acnt pad)
                                           a
                                           (into a (take (- pad acnt)) (repeat nil)))
                                       b (if (= bcnt pad)
                                           b
                                           (into b (take (- pad bcnt)) (repeat nil)))]
                                   (compare a b)))))]
    (sort icompare xs)))


(defn scp4 [xs]
  (let [pcnt (reduce (fn [m x] (if (coll? x) (let [cnt (count x)] (if (> cnt m) cnt m)) m))
                     1
                     xs)
        pad (fn [x] (cond (vector? x) (into x (take (- pcnt (count x))) (repeat nil))
                          (coll? x) (concat x (repeat (- pcnt (count x)) nil))
                          :else x))
        icompare (fn [a b]
                   (let [a1? (number? a)
                         b1? (number? b)]
                     (cond (and a1? b1?) (compare a b)
                           a1? (if (<= a (first b)) -1 1)
                           b1? (if (< (first a) b) -1 1)
                           :else (compare a b))))]
    (sort-by pad icompare xs)))



(defn scp3 [xs]
  (let [icompare (fn [a b]
                   (let [a1? (number? a)
                         b1? (number? b)]
                     (cond (and a1? b1?) (compare a b)
                           a1? (if (<= a (first b)) -1 1)
                           b1? (if (< (first a) b) -1 1)
                           :else (let [acnt (count a)
                                       bcnt (count b)
                                       pad (max acnt bcnt)
                                       ap (if (= acnt pad)
                                           a
                                           (into (vec a) (take (- pad acnt)) (repeat nil)))
                                       bp (if (= bcnt pad)
                                           b
                                           (into (vec b) (take (- pad bcnt)) (repeat nil)))]
                                   (compare ap bp)))))]
    (sort icompare xs)))


(defn scp2 [xs]
  (let [icompare (fn [a b]
                   (let [a1? (number? a)
                         b1? (number? b)]
                     (cond (and a1? b1?) (compare a b)
                           a1? (if (<= a (first b)) -1 1)
                           b1? (if (< (first a) b) -1 1)
                           :else (let [acnt (count a)
                                       bcnt (count b)
                                       pad (max acnt bcnt)
                                       a (into a (take (- pad acnt)) (repeat  nil))
                                       b (into b (take (- pad bcnt)) (repeat  nil))]
                                   (compare a b)))))]
    (sort icompare xs)))


(defn scw [xs]
  (let [icomp (fn [a b]
                (loop [a (seq a) b (seq b)]
                  (let [a1 (first a)
                        b1 (first b)
                        c (compare a1 b1)]
                    (if (zero? c)
                      (if (and (nil? a1) (nil? b1))
                        0
                        (recur (rest a) (rest b)))
                      c))))
        wrap (fn [x] (if (number? x)
                       (list x Long/MIN_VALUE)
                       (concat x (list Long/MIN_VALUE Long/MIN_VALUE))))]
    (sort-by wrap icomp xs)))

(defn scw2 [xs]
  (let [icomp (fn [a b]
                (loop [a (seq a) b (seq b)]
                  (let [a1 (first a)
                        b1 (first b)
                        c (compare a1 b1)]
                    (if (zero? c)
                      (if (and (empty? a) (empty? b))
                        0
                        (recur (rest a) (rest b)))
                      c))))
        wrap (fn [x] (if (number? x)
                       (list x)
                       (concat x (list Long/MIN_VALUE))))]
    (sort-by wrap icomp xs)))


;; a bit faster if you assume vectors, but still not even close to my favorite
(defn scw3 [xs]
  (let [icomp (fn [a b]
                (loop [a (seq a) b (seq b)]
                  (let [a1 (first a)
                        b1 (first b)
                        c (compare a1 b1)]
                    (if (zero? c)
                      (if (and (empty? a) (empty? b))
                        0
                        (recur (rest a) (rest b)))
                      c))))
        wrap (fn [x] (if (vector? x)
                       (conj x Long/MIN_VALUE)
                       (vector x)))]
    (sort-by wrap icomp xs)))



;;; lots of variations but not better or faster
;;; many had the infinite loop bug so I deleted them


;; Issue: built-in compare checks size of vectors first, then does element by element
;; compare.  That forces us to pad appropriately if we want to use standard compare.  But
;; it's slow.

;; padding is much slower
(defn sbyc [xs]
  (let [pad (reduce (fn [r x] (let [c (if (coll? x) (count x) 1)] (if (> c r) c r)))
                    0
                    xs)
        extend (fn [x] (if (coll? x)
                   (into [(first x)] (take pad) (concat x (repeat nil)))
                   (into [x (dec x)] (repeat (dec pad) nil))))]
    (sort-by extend xs)))






(defn smoke-sort
  ([] (smoke-sort sort-by-content))
  ([sort-by-content]
   (assert (= (sort-by-content [4 5 3 2 1]) [1 2 3 4 5]))
   (assert (= (sort-by-content [[2 3] [0 9] [-1 3 4] [0 3]]) [[-1 3 4] [0 3] [0 9] [2 3]]))
   (assert (= (sort-by-content [5 [4 5] 3 1 [0 2 3]]) [[0 2 3] 1 3 [4 5] 5]))
   (assert (= (sort-by-content [[1] 1])  [1 [1]]))
   (assert (= (sort-by-content [[10 3 4 1]]) [[10 3 4 1]]))
   (assert (= (sort-by-content [[1] [1]])  [[1] [1]]))
   (assert (= (sort-by-content [[1] [1 2]]) [[1] [1 2]]))
   (assert (= (sort-by-content [[1 2] [1]]) [[1] [1 2]]))
   true))






;;; copied and reformatted for my testing.  Much slower than mine.
(defn westcott-sbc [xs]
  (let [wrap (fn [x] (if (number? x) [x] x))
        pad (fn [n xs]  (vec (take n (concat xs (repeat nil)))))
        comp* (fn [x y]
                (let [xs (wrap x)
                      ys (wrap y)
                      len (max (count xs) (count ys))
                      cmp (compare (pad len xs) (pad len ys))]
                  (if (zero? cmp)
                    (case [(number? x) (number? y)]
                      [false false] 0
                      [false true] 1
                      [true false] -1
                      [true true] 0)
                    cmp)))]
    (sort comp* xs)))



(defn mendel-sbc [xs]
  (let [content-comparator
        (fn [x y]
          (let [[a b] (first
                       (drop-while (fn [[a b]] (= a b))
                                   (map vector
                                        (if (coll? x) x [x])
                                        (if (coll? y) y [y]))))]
            (if a
              (if (< a b) -1 (if (> a b) 1 0))
              (if (coll? x) 1 -1))))]
    (sort-by identity content-comparator xs)))
