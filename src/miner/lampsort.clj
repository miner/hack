(ns miner.lampsort)

;;; inspirted by "Lampsort"
;;; https://bertrandmeyer.com/2014/12/07/lampsort/

;;; Hacker News discussion
;;; https://news.ycombinator.com/item?id=8712879

;;; This is not practical.  Just trying to understand the article.  Much slower than
;;; built-in sort.  The point seems to be that the unsorted intervals could be handled by
;;; independent threads in parallel, although that is not implemented here.


;;; ported frm Python implementation
;;; https://gist.github.com/joelgrus/9dc47ebb22243fe990e5

;;; Be careful, there may be some fence post errors in my translation

;;; a is a vector of integers, lo and hi are indices, inclusive
;;; returns (conj modified-a pivot), that is pivot at peek
(defn partition-pivot [a lo hi]
  (let [piv (a hi)]
    (loop [a a i lo at lo]
      (cond (>= i hi) (conj (assoc a hi (a at) at (a hi)) at)
            (< (a i) piv) (recur (if (= i at) a (assoc a i (a at) at (a i))) (inc i) (inc at))
            :else (recur a (inc i) at)))))

;;; intervals are the inclusive indices of unsorted regions
;;; faster with list of intervals (vs original set or vector)
;;; the pivot can be left in place
;;; refactored by SEM
(defn lampsort [a]
  (let [cnt (count a)]
    (if (<= cnt 1)
      a
      (loop [a a intervals (list [0 (dec cnt)])]
        (if-let [[lo hi] (peek intervals)]
          (let [ap (partition-pivot a lo hi)
                p (peek ap)]
            (recur (pop ap)
                   (cond-> (pop intervals)
                     (> (dec p) lo) (conj [lo (dec p)])
                     (> hi (inc p)) (conj [(inc p) hi]))))
          a)))))

(defonce samples (take 100 (iterate shuffle (vec (range 30 90 3)))))

(defn test-lampsort
  ([] (test-lampsort lampsort))
  ([lampsort]
   (assert (= (lampsort []) []))
   (assert (= (lampsort [42]) [42]))
   (assert (= (lampsort [4 2]) [2 4]))
   (assert (= (lampsort [2 4]) [2 4]))
   (let [sol (sort (first samples))]
     (assert (every? #(= sol (lampsort %)) samples)))
   true))

(defn partition-pivot2-SAVE [a lo hi]
  (let [piv (a hi)]
    (loop [a a i lo at lo]
      (cond (>= i hi) (conj (if (= hi at) a (assoc a hi (a at) at (a hi))) at)
            (< (a i) piv) (recur (if (= i at) a (assoc a i (a at) at (a i))) (inc i) (inc at))
            :else (recur a (inc i) at)))))

;;; Slower to use aat = [a... at]  peek holding at
(defn partition-pivot2-slow [a lo hi]
  (let [piv (a hi)
        rat (reduce (fn [aat i]
                      (let [at (peek aat)]
                        (if (< (aat i) piv)
                          (conj (if (= i at) (pop aat) (assoc (pop aat) i (aat at)
                                                              at (aat i)))
                                    (inc at))
                          aat)))
            (conj a lo)
            (range lo (inc hi)))]
    (assoc rat hi (rat (peek rat)) (peek rat) (rat hi))))


(defn partition-pivot2-slower [a lo hi]
  (let [piv (a hi)
        lha (subvec a lo hi)
        b (into (subvec a 0 lo) (filter #(< % piv)) lha)
        cnt (count b)]
    (-> b
        (conj piv)
        (into (filter #(> % piv)) lha)
        (into (subvec a (inc hi)))
        (conj cnt))))



(defn partition-pivot2-slowest [a lo hi]
  (let [piv (a hi)
        lha (subvec a lo hi)
        b1 (vec (filter #(< % piv) lha))
        at (+ lo (count b1))
        b (into (conj b1 piv) (filter #(> % piv)) lha)]
    (conj (into (into (subvec a 0 lo) b) (subvec a (inc hi))) at)))


(defn partition-pivot2-almost-slowest [a lo hi]
  (let [piv (a hi)
        lha (subvec a lo hi)
        b1 (filterv #(< % piv) lha)
        at (+ lo (count b1))
        b (into (conj b1 piv) (filter #(> % piv)) lha)]
    (into (subvec a 0 lo)
          cat
          (list b (subvec a (inc hi)) (list at)))))

;;; TOTAL MESS
(defn partition-pivot2 [a lo hi]
  (let [piv (a hi)
        b1 (tranduce (comp (filter #(< % piv)) (map-indexed vector))
              (fn ([r] (conj r piv))
                ([r [i x]] (assoc r (+ lo i) x)))
              a
              (subvec a lo hi))


    (into (subvec a 0 lo)
          cat
          (list b (subvec a (inc hi)) (list at)))))

;;; intervals are the inclusive indices of unsorted regions
;;; faster with list of intervals (vs original set or vector)
;;; the pivot can be left in place
;;; refactored by SEM
(defn lampsort2 [a]
  (let [cnt (count a)]
    (if (<= cnt 1)
      a
      (loop [a a intervals (list [0 (dec cnt)])]
        (if-let [[lo hi] (peek intervals)]
          (let [ap (partition-pivot2 a lo hi)
                p (peek ap)]
            (recur (pop ap)
                   (cond-> (pop intervals)
                     (> (dec p) lo) (conj [lo (dec p)])
                     (> hi (inc p)) (conj [(inc p) hi]))))
          a)))))

