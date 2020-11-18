(ns miner.esort)

;;; https://gist.github.com/ericnormand/48b0a933294639bb27902eda062772e2

;;; In this task, we are sorting a heterogeneous list of elements. The elements are either
;;; single numbers or sequences of numbers. We should sort them by numeric content, like
;;; this:  we sort sequences by their first element, then by second element, then by third,
;;; etc; we sort numbers and sequences together as if numbers were sequences of length 1;
;;; but numbers sort before sequences when the number is the same as the first element.



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
                                     (if-not (zero? c)
                                       c
                                       (recur (rest a) (rest b))))))))]
    (sort icompare xs)))



;; originally submitted but changed to use number?
(defn submitted-sort-by-content [xs]
  (let [icompare (fn [a b]
                   (let [a1? (not (coll? a))
                         b1? (not (coll? b))]
                     (cond (and a1? b1?) (compare a b)
                           a1? (if (<= a (first b)) -1 1)
                           b1? (if (< (first a) b) -1 1)
                           :else (loop [a (seq a) b (seq b)]
                                   (let [c (compare (first a) (first b))]
                                     (if-not (zero? c)
                                       c
                                       (recur (rest a) (rest b))))))))]
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



;;; lots of variations but not better or faster


(defn icompare2 [a b]
  (cond (and (coll? a) (coll? b)) (let [c (compare (first a) (first b))]
                                    (if (zero? c) (recur (rest a) (rest b)) c))
        (coll? a) (if (< (first a) b) -1 1)
        (coll? b) (if (<= a (first b)) -1 1)
        :else (compare a b)))



(defn icomp [a b]
  (let [as? (coll? a)
        bs? (coll? b)]
    (if (and as? bs?)
      (loop [a (seq a) b (seq b)]
        (let [c (compare (first a) (first b))]
          (if-not (zero? c)
            c
            (recur (rest a) (rest b)))))
      (cond as? (if (< (first a) b) -1 1)
            bs? (if (<= a (first b)) -1 1)
            :else (compare a b)))))

(defn sbyi [xs]
  (sort icomp xs))




(defn icmp [a b]
  (let [a1? (not (coll? a))
        b1? (not (coll? b))]
    (cond (and a1? b1?) (compare a b)
          a1? (if (<= a (first b)) -1 1)
          b1? (if (< (first a) b) -1 1)
          :else (loop [a (seq a) b (seq b)]
                  (let [c (compare (first a) (first b))]
                    (if-not (zero? c)
                      c
                      (recur (rest a) (rest b))))))))

(defn sbyz [xs]
  (sort icmp xs))









;; padding is much slower
(defn sbyc [xs]
  (let [pad (reduce (fn [r x] (let [c (if (coll? x) (count x) 1)] (if (> c r) c r)))
                    0
                    xs)
        extend (fn [x] (if (coll? x)
                   (into [(first x)] (take pad) (concat x (repeat nil)))
                   (into [x (dec x)] (repeat (dec pad) nil))))]
    (sort-by extend xs)))




;; assuming vectors is only a tiny bit faster than seqs so not worth it
(defn vcompare [a b]
  (cond (and (vector? a) (vector? b)) (let [c (compare (nth a 0 nil) (nth b 0 nil))]
                                        (if (zero? c) (recur (subvec a 1) (subvec b 1)) c))
        (vector? a) (if (< (nth a 0) b) -1 1)
        (vector? b) (if (<= a (nth b 0)) -1 1)
        :else (compare a b)))


(defn sbyv [xs]
  (sort vcompare xs))








(defn smoke-sort
  ([] (smoke-sort sort-by-content))
  ([sort-by-content]
   (assert (= (sort-by-content [4 5 3 2 1]) [1 2 3 4 5]))
   (assert (= (sort-by-content [[2 3] [0 9] [-1 3 4] [0 3]]) [[-1 3 4] [0 3] [0 9] [2 3]]))
   (assert (= (sort-by-content [5 [4 5] 3 1 [0 2 3]]) [[0 2 3] 1 3 [4 5] 5]))
   (assert (= (sort-by-content [[1] 1])  [1 [1]]))
   (assert (= (sort-by-content [[10 3 4 1]]) [[10 3 4 1]]))
   true))



