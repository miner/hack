(ns eric.unfriendly)

;; https://gist.github.com/ericnormand/598b0fc7aea99b628cfc7120857dc7df

;;; Unfriendly neighors

;;; Let's say we have a sequence of integers: [1 3 2 4 1]
;;; There are 4 spots between numbers where we could insert a new number (represented by commas):

[1 ,, 3 ,, 2 ,, 4 ,, 1]
;; ^    ^    ^    ^
;; |    |    |    spot 3.5, between 3 and 
;; |    |    spot 2.5, between 2 and 3
;; |    spot 1.5, between 1 and 2
;; spot 0.5, between 0 and 1

;; We can represent those spots as halves, such as 0.5. We only want to insert numbers when
;; they are "happy". Odd numbers are happy when we insert them next to at least one odd
;; number. Even numbers are happy when we insert them next to at least one even
;; number. Write a function that takes a sequence of integers and an integer. It should
;; return the happy spots and the unhappy spots. Note: Inserting a number may make
;; pre-existing numbers unhappy. Ignore them!



;; actually a bit faster with standard, transients not worth it

(defn spots [xs x]
  (let [test? (if (odd? x) odd? even?)
        xv (if (vector? xs) xs (into [] xs))]
    (reduce (fn [m i]
              (let [k (if (or (test? (xv i)) (test? (xv (inc i)))) :happy :unhappy)]
                (assoc m k (conj (m k []) (+ i 0.5)))))
            {}
            (range (dec (count xv))))))


;;; Idea:  only two adjacent wrong parity are unhappy, otherwise happy


;; slow 2x
(defn uspots [xs x]
  (let [unh? (if (odd? x) even? odd?)
        unv (mapv (fn [a b] (and (unh? a) (unh? b))) xs (rest xs))]
    (reduce (fn [m i]
              (let [k (if (unv i) :unhappy :happy)]
                (assoc m k (conj (m k []) (+ i 0.5)))))
            {}
            (range (count unv)))))

;; fast, = spots
(defn uspots2 [xs x]
  (let [unh? (if (odd? x) even? odd?)
        xv (if (vector? xs) xs (into [] xs))]
    (reduce (fn [m i]
              (let [k (if (and (unh? (xv i)) (unh? (xv (inc i)))) :unhappy :happy)]
                (assoc m k (conj (m k []) (+ i 0.5)))))
            {}
            (range (dec (count xv))))))

(defn uspots3 [xs x]
  (let [unh? (if (odd? x) even? odd?)
        xv (if (vector? xs) xs (into [] xs))]
    (reduce (fn [m i]
              (let [k (if (and (unh? (xv i)) (unh? (xv (inc i)))) :unhappy :happy)]
                (assoc m k (conj (m k []) (+ i 0.5)))))
            {}
            (range (dec (count xv))))))

;; slow 2x
(defn kspots [xs x]
   (let [test? (if (odd? x) odd? even?)]
   (reduce (fn [m [k i]]
            (assoc m k (conj (m k []) i)))
          {}
          (map (fn [i a b]
                 (let [k (if (or (test? a) (test? b)) :happy :unhappy)]
                   [k (+ i 0.5)]))
               (range) xs (rest xs)))))


;; clever but not faster.  Even sum = same e/e odd/odd
(defn espots [xs x]
  (let [xv (if (vector? xs) xs (into [] xs))]
    (reduce (fn [m i]
              (let [k (if (or (even? (+ (xv i) x)) (even? (+ (xv (inc i)) x))) :happy :unhappy)]
                (assoc m k (conj (m k []) (+ i 0.5)))))
            {}
            (range (dec (count xv))))))

;; but slower!
(defn espots2 [xs x]
  (let [tv (mapv #(even? (+ % x)) xs)]
    (reduce (fn [m i]
              (let [k (if (or (tv i) (tv (inc i))) :happy :unhappy)]
                (assoc m k (conj (m k []) (+ i 0.5)))))
            {}
            (range (dec (count tv))))))



;; custom odd/even not faster
(defn modd? [n] (bit-test n 0))
(defn meven? [n] (not (modd? n)))

(defn mspots [xs x]
  (let [test? (if (modd? x) modd? meven?)
        xv (if (vector? xs) xs (into [] xs))]
    (reduce (fn [m i]
              (let [k (if (or (test? (xv i)) (test? (xv (inc i)))) :happy :unhappy)]
                (assoc m k (conj (m k []) (+ i 0.5)))))
            {}
            (range (dec (count xv))))))



(defn spots-OK [xs x]
  (let [test? (if (odd? x) odd? even?)
        tv (mapv test? xs)]
    (reduce (fn [m i]
              (let [k (if (or (tv i) (tv (inc i))) :happy :unhappy)]
                (assoc m k (conj (m k []) (+ i 0.5)))))
            {}
            (range (dec (count tv))))))


;; x2 slower
(defn spots8 [xs x]
  (let [test? (if (odd? x) odd? even?)
        tv (mapv #(or (test? %) (test? %2)) xs (rest xs))]
    (reduce (fn [m i]
              (let [k (if (tv i) :happy :unhappy)]
                (assoc m k (conj (m k []) (+ i 0.5)))))
            {}
            (range (count tv)))))


;; slow again
(defn spots7 [xs x]
  (let [test? (if (odd? x) odd? even?)
        ts (map test? xs)
        res (sequence (map (fn [a b s] (if (or a b) s (- s))))
                      ts
                      (rest ts)
                      (range 0.5 (count ts) 1.0))
        happy (filterv pos? res)
        unhappy (mapv - (filter neg? res))]
    (cond (and (seq happy) (seq unhappy)) {:happy happy :unhappy unhappy}
          (seq happy)  {:happy happy}
          (seq unhappy) {:unhappy unhappy}
          :else {})))






(defn spots1 [xs x]
  (let [test? (if (odd? x) odd? even?)
        iscore (map (fn [a b i] (if (or (test? a) (test? b)) i (- i))) xs (rest xs)
                    (map inc (range)))
        happy (mapv #(- % 0.5) (filterv (complement neg?) iscore))
        unhappy (mapv #(- (- %) 0.5) (filterv neg? iscore))]
    (cond (and (seq happy) (seq unhappy)) {:happy happy :unhappy unhappy}
          (seq happy)  {:happy happy}
          (seq unhappy) {:unhappy unhappy}
          :else {})))


(defn vconj [v x] (if (nil? v) [x] (conj v x)))

(defn spots2 [xs x]
  (let [test? (if (odd? x) odd? even?)
        tv (mapv test? xs)]
    (reduce (fn [m i] (if (or (tv i) (tv (inc i)))
                        (update m :happy vconj (+ i 0.5))
                        (update m :unhappy vconj (+ i 0.5))))
            {}
            (range (dec (count tv))))))


;; was fastest so far
(defn spots4 [xs x]
  (let [test? (if (odd? x) odd? even?)
        tv (mapv test? xs)]
    (reduce (fn [m i]
              (update m (if (or (tv i) (tv (inc i))) :happy :unhappy) (fnil conj []) (+ i 0.5)))
            {}
            (range (dec (count tv))))))


;; slightly faster but ultimately not worth transients for small examples
(defn spots41 [xs x]
  (let [test? (if (odd? x) odd? even?)
        tv (mapv test? xs)]
    (persistent!
     (reduce (fn [m i]
              (let [k (if (or (tv i) (tv (inc i))) :happy :unhappy)]
                (assoc! m k (conj (m k []) (+ i 0.5)))))
            (transient {})
            (range (dec (count tv)))))))



;; very slow
(defn spots5 [xs x]
  (let [test? (if (odd? x) odd? even?)
        spots (range 0.5 (count xs) 1.0)
        haps (sequence (map (fn [a b s] (when (or (test? a) (test? b)) s)))
                        xs
                        (rest xs)
                        spots)
        unhaps (sequence (comp (map (fn [h s] (when-not h s))) (remove nil?)) haps spots)
        happy (into [] (remove nil?) haps)]
    (cond (and (seq happy) (seq unhaps)) {:happy happy :unhappy (vec unhaps)}
          (seq happy)  {:happy happy}
          (seq unhaps) {:unhappy (vec unhaps)}
          :else {})))
               



(defn smoke-spots [spots]
  (assert (= (spots [1 1]     1) {:happy [0.5]                   }))
  (assert (= (spots [1 1]     2) {                 :unhappy [0.5]}))
  (assert (= (spots [1 1 2]   4) {:happy [1.5]     :unhappy [0.5]}))
  (assert (= (spots [1 1 2 2] 3) {:happy [0.5 1.5] :unhappy [2.5]}))
  true)


;;; nice but slow
(defn sw-spots [xs n]
  (let [m (->> (map #(even? (+ n %)) xs)
               (partition 2 1)
               (map #(if (some identity %) :happy :unhappy))
               (map vector (iterate inc 0.5))
               (group-by second))]
    (update-vals m #(map first %))))

;; still slow
(defn nb-spots [numbers candidate]
  (let [good-neighbor? (if (odd? candidate) odd? even?)
        pairs (partition 2 1 numbers)
        spots (range 0.5 (count numbers))]
    (->> (for [[neighbors spot] (map vector pairs spots)]
           (if (some good-neighbor? neighbors)
             {:happy [spot]}
             {:unhappy [spot]}))
         (reduce (partial merge-with into)))))
