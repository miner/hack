(ns eric.oddone)

;; https://gist.github.com/ericnormand/c2c2cf0a249224de8a134e1d90b78c0b

;;; Odd One Out.  Write a function that takes a list of words (Strings). The function should
;;; return true if exactly 1 word differs in length from the others. It should return false
;;; in all other cases.

;;; what about a single word?  Consider others = 0, seems good.

(defn smoke-odd [odd-one?]
  (assert (not (odd-one? ["a" "b" "c"])))
  (assert (not (odd-one? ["a" "b"])))
  (assert (not (odd-one? [])))
  (assert (odd-one? ["a" "ab"]))
  (assert (odd-one? ["abc" "ab"]))
  (assert (odd-one? ["a" "b" "cc"]))
  (assert (odd-one? ["abc" "aa" "xy" "jj"]))
  (assert (odd-one? ["foo"]))
  (assert (odd-one? ["one" "two" "three"]))
  (assert (not (odd-one? ["abc" "aa" "xyz" "jj"])))
  true)


;; better to use `some` than `reduce min ...`

(defn odd-one? [words]
  (let [fqs (frequencies (map count words))]
    (and (<= (count fqs) 2)
         (some #(= % 1) (vals fqs)))))

(defn odd-one-SUBMITTED? [words]
  (let [fqs (frequencies (map count words))]
    (and (<= (count fqs) 2)
         (= (reduce min 2 (vals fqs)) 1))))

;; revised submission
(defn todd-one? [words]
  (transduce (map count)
             (fn ([m c]
                  (if-let [cnt (m c)]
                    (if (> cnt 1)
                      m
                      (if (every? #(= % 1) (vals m))
                        (assoc m c 2)
                        (reduced nil)))
                    (if (< (count m) 2)
                      (assoc m c 1)
                      (reduced nil))))
               ([m] (some #(= % 1) (vals m))))
             {}
             words))

;; I liked the look of this but it's actually slower
(defn todd-one4? [words]
  (transduce (map count)
             (fn ([m c]
                  (if-let [cnt (m c)]
                    (if (> cnt 1)
                      m
                      (if (every? #(= (val %) 1) m)
                        (assoc m c 2)
                        (reduced nil)))
                    (if (< (count m) 2)
                      (assoc m c 1)
                      (reduced nil))))
               ([m] (some #(= (val %) 1) m)))
             {}
             words))

(defn todd-one-SUBMITTED? [words]
  (transduce (map count)
             (fn ([m c]
                  (if-let [cnt (m c)]
                    (if (> cnt 1)
                      m
                      (if (every? #(= % 1) (vals m))
                        (assoc m c 2)
                        (reduced nil)))
                    (if (< (count m) 2)
                      (assoc m c 1)
                      (reduced nil))))
               ([m] (= (reduce min 2 (vals m)) 1)))
             {}
             words))



;; faster but not submitted
(defn rodd-one? [words]
  (->> (reduce (fn [m word]
                      (let [cnt (count word)]
                        (if-let [c (m cnt)]
                          (if (> c 1)
                            m
                            (if (every? #(= % 1) (vals m))
                              (assoc m cnt 2)
                              (reduced nil)))
                          (if (< (count m) 2)
                            (assoc m cnt 1)
                            (reduced nil)))))
                    {}
                    words)
       vals  
       (some #(= % 1))))


;; doesn't work with single string, or two string a ab
(defn sw-odd-one? [strings]
  (->> strings
       (map count)
       frequencies
       vals
       set
       (= #{1 (dec (count strings))})))

;; doesn't work with two string: a ab
(defn se-odd-one? [words]
  (-> (map count words)
      frequencies
      vals
      frequencies
      (get 1)
      (= 1)))
