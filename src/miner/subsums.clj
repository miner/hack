(ns miner.subsums
  (:require [clojure.math.combinatorics :as mc]))


;; https://purelyfunctional.tv/issues/purelyfunctional-tv-newsletter-372-model-change-over-time-with-state-machines/

;; challenge

;; (inspired by Eric Normand)
;; Eager, not "standard" order, but fast.
;; Consider clojure.math.combinatorics/subsets if you want lazy and standard order.
(defn power-seqs [coll]
  (reduce (fn [res x] (into res (map #(conj % x)) res)) (list ()) coll))

(defn subset-sums1 [nums sum]
  (into #{} 
        (comp (filter #(zero? (reduce - sum %)))
              (map set))
        (power-seqs nums)))


;; fast
(defn subset-sums [nums sum]
  (into #{} 
        (comp (filter #(zero? (reduce - sum %)))
              (map set))
        (reduce (fn [ss x] (into ss (map #(conj % x)) ss)) (list ()) nums)))

;; add comment to
;; https://gist.github.com/ericnormand/6e1a9d9135fc4f5eb7776066e4db9de7

(defn mc-subset-sums [nums sum]
  (into #{} 
        (comp (filter #(zero? (reduce - sum %)))
              (map set))
        (mc/subsets (vec nums))))


;; pretty good and clear, but translated into transducer (above) is faster
(defn subset-sums10 [nums sum]
  (->> nums
       power-seqs
       (filter #(zero? (reduce - sum %)))
       (map set)
       set))


#_
(require '[clojure.math.combinatorics :as mc])

(defn subset-sums11 [nums sum]
  (->> nums
       vec
       mc/subsets
       (filter #(zero? (reduce - sum %)))
       (map set)
       set))

(defn subset-sums22 [nums sum]
  (->> nums
       vec
       mc/subsets
       (filter #(= sum (reduce + 0 %)))))


;; self-contained, without libraries, somewhat slower.
;; gen-indices is similar to mc/combinations for (range n) -- that is, returning just the
;; indicies of the combinations. Returns set of sets as specified.
(defn sb-sums [nums sum]
  (let [v (vec nums)
        cnt (count v)
        gen-indices (fn [choose]
                      ;; {:pre [(pos-int? choose) (<= choose cnt)]}
                      (loop [i (dec choose) res (mapv vector (range cnt))]
                        (if (zero? i)
                          res
                          (recur (dec i)
                                 (into []
                                       (mapcat (fn [prev]
                                                 (eduction (map #(conj prev %))
                                                           (range (inc (peek prev)) cnt))))
                                       res)))))]
    (into (if (zero? sum) #{#{}} #{})
          (comp
           (mapcat gen-indices)
           (filter #(zero? (reduce (fn [r i] (- r (v i))) sum %)))
           (map #(map v %))
           (map set))
          (range 1 (inc cnt)))))





(defn sb-sums0 [nums sum]
  (let [v (vec nums)
        cnt (count v)
        gen-indices (fn [choose]
                      ;; {:pre [(pos-int? choose) (<= choose cnt)]}
                      (loop [i (dec choose) res (mapv vector (range cnt))]
                        (if (zero? i)
                          res
                          (recur (dec i)
                                 (into []
                                       (mapcat (fn [prev]
                                                 (eduction (map #(conj prev %))
                                                           (range (inc (peek prev)) cnt))))
                                       res)))))]
    (into (if (zero? sum) [()] [])
          (comp
           (mapcat gen-indices)
           (filter #(zero? (reduce (fn [r i] (- r (v i))) sum %)))
           (map #(map v %)))
          (range 1 (inc cnt)))))





;; like mc/combinations for just the item indices
(defn choose-indices [cnt choose]
 ;; {:pre [(pos-int? choose) (<= choose cnt)]}
 (loop [i (dec choose) res (mapv vector (range cnt))]
   (if (zero? i)
     res
     (recur (dec i)
            (into []
                  (mapcat (fn [prev]
                            (eduction (map #(conj prev %)) (range (inc (peek prev)) cnt))))
                  res)))))






;; map only while adding,
;; still slower than obvious mc version
(defn subset-sums34 [nums sum]
  (let [v (vec nums)
        cnt (count v)]
    (into (if (zero? sum) [()] [])
          (comp
           (mapcat #(choose-indices cnt %))
           (filter #(zero? (reduce (fn [r i] (- r (v i))) sum %)))
           (map #(map v %)))
          (range 1 (inc cnt)))))







;; all sets all the time
(defn subset-sums36 [nums sum]
  (let [v (vec nums)
        cnt (count v)]
    (into (if (zero? sum) #{()} #{})
          (comp
           (mapcat #(choose-indices cnt %))
           (filter #(zero? (reduce (fn [r i] (- r (v i))) sum %)))
           (map #(into #{} (map v) %)))
          (range 1 (inc cnt)))))


;; slightly faster than obvious version, much more complicated
;; uses mc/combinations which is faster than my my choose-indices
(defn subset-sums46 [nums sum]
  (let [v (vec nums)
        cnt (count v)]
    (into (if (zero? sum) #{()} #{})
          (comp
           (mapcat #(mc/combinations (range cnt) %))
           (filter #(zero? (reduce (fn [r i] (- r (v i))) sum %)))
           (map #(into #{} (map v) %)))
          (range 1 (inc cnt)))))






(defn subset-sums12 [nums sum]
  (into #{}
        (comp
         (filter #(zero? (reduce - sum %)))
         (map set))
        (mc/subsets (vec nums))))

(defn subset-sums1 [nums sum]
  (set (map set (filter #(= (reduce + 0 %) sum) (mc/subsets (vec nums))))))

;; mc/subsets doesn't work with sets!  Must seq or vec nums input.  Vec is slightly faster.


;; basically the same
(defn subset-sums2 [nums sum]
  (filter #(= (reduce + 0 %) sum) (mc/subsets (vec nums))))

(defn subset-sums3 [nums sum]
  (sequence (filter #(= (reduce + 0 %) sum)) (mc/subsets (vec nums))))

(defn subset-sums4 [nums sum]
  (into #{} (comp (filter #(zero? (reduce - sum %))) (map set)) (mc/subsets (vec nums))))



(defn set= [a b]
  (= (set (map set a)) (set (map set b))))

;; SEM: Note Eric gave two incomplete examples!

(defn smoke-subs
  ([] (smoke-subs subset-sums))
  ([subset-sums]
   (let [set= (fn [a b] (= (set (map set a)) (set (map set b))))]
     (assert (set= (subset-sums #{1 2 3} 0) #{ #{} }))
     (assert (set= (subset-sums #{1 2 3 4 5} 6)  #{ #{1 5} #{2 4} #{1 3 2}} ))
     (assert (set= (subset-sums #{1 2 3 5 6 7} 7)  #{ #{1 6} #{2 5} #{7} }))
     (assert (set= (subset-sums #{0 1 -1} 0) #{ #{} #{0} #{1 -1} #{0 1 -1} }))
     (assert (set= (subset-sums (range 10) 42)
                   #{#{7 1 4 6 2 9 5 8} #{0 7 4 6 3 9 5 8} #{7 4 6 3 9 5 8}
                     #{0 7 1 4 6 2 9 5 8}}))
     true)))


(defn ben-subs [subset-sums]
  (reduce (fn [r ss] (reduce + r ss))
          0
          (concat (subset-sums #{1 2 3} 0)
                (subset-sums #{1 2 3 4 5} 6)
                (subset-sums #{1 2 3 5 6 7} 7)
                (subset-sums #{0 1 -1} 0)
                (subset-sums (range 10) 42))))



;; https://gist.github.com/ericnormand/6e1a9d9135fc4f5eb7776066e4db9de7
;; Eric's solution
(defn Esubsets [s]
  (loop [ret #{#{}} rem s]
    (if (empty? rem)
      ret
      (let [[f & rst] rem]
        (recur (into ret (map #(conj % f)) ret) rst)))))

(defn Esubset-sums [s n]
  (->> s
       Esubsets
       (filter #(= n (reduce + 0 %)))
       set))




(defn semsets1 [s]
  (loop [ret #{#{}} rem (seq s)]
    (if (seq rem)
      (recur (into ret (map #(conj % (first rem))) ret) (rest rem))
      ret)))

(defn semsets [coll]
  (loop [ret #{#{}} rem (seq coll)]
    (if rem
      (recur (into ret (map #(conj % (first rem))) ret) (next rem))
      ret)))




(defn subseqs1 [coll]
  (loop [ret (list ()) rem (seq coll)]
    (if rem
      (recur (into ret (map #(conj % (first rem))) ret) (next rem))
      ret)))


(defn sb-sums10 [nums sum]
  (->> nums
       subseqs
       (filter #(zero? (reduce - sum %)))))




(defn OLD-choose-indices [cnt choose]
  ;; {:pre [(pos-int? choose) (<= choose cnt)]}
  (loop [i (dec choose) res (map vector (range cnt))]
    (if (zero? i)
      res
      (recur (dec i)
             (mapcat (fn [prev] (map #(conj prev %)
                                     (range (inc (peek prev)) cnt)))
                     res)))))







;; mc/subsets doesn't work with sets!  Must seq or vec nums input.  Vec is slightly faster.

;; inside mc/combinations calls distinct, which doesn't work on a set!

;; (distinct #{1 2 3})
;; Error printing return value (UnsupportedOperationException) at
;; clojure.lang.RT/nthFrom (RT.java:991). nth not supported on this type: PersistentHashSet

;; (let [[f :as xs] #{1 2 3}] xs)
;; Execution error (UnsupportedOperationException) at user/eval2925 (REPL:1).
;; nth not supported on this type: PersistentHashSet



;; SEM hacked version of c.c/distinct

(defn dist-orig [coll]
   (let [step (fn step [xs seen]
                (lazy-seq
                  ((fn [[f :as xs] seen]
                     (when-let [s (seq xs)]
                       (if (contains? seen f)
                         (recur (rest s) seen)
                         (cons f (step (rest s) (conj seen f))))))
                   xs seen)))]
       (step coll #{})))

(defn dist1 [coll]
   (let [step (fn step [xs seen]
                (lazy-seq
                  ((fn [[f :as xs] seen]
                     (when-let [s (seq xs)]
                       (if (contains? seen f)
                         (recur (rest s) seen)
                         (cons f (step (rest s) (conj seen f))))))
                   xs seen)))]
     (if (set? coll)
       (seq coll)
       (step coll #{}))))

;; SEM run built-in for transducer
(defn dist
  ([] (distinct))
  ([coll]
  (let [step (fn step [xs seen]
               (lazy-seq
                ((fn [xs seen]
                   (when-let [s (seq xs)]
                     (let [f (first s)]
                       (if (contains? seen f)
                         (recur (rest s) seen)
                         (cons f (step (rest s) (conj seen f)))))))
                 xs seen)))]
    (step coll #{}))))

(defn dist2 [coll]
  (let [step (fn step [xs seen]
               (lazy-seq
                ((fn [xs seen]
                   (when-let [s (seq xs)]
                     (let [f (first s)]
                       (if (contains? seen f)
                         (recur (rest s) seen)
                         (cons f (step (rest s) (conj seen f)))))))
                 xs seen)))]
    (if (set? coll)
      (seq coll)
      (step coll #{}))))


(defn assert-result
  ([f x expected] (assert (= (f x) expected)) true)
  ([f x expected & more] (and (assert-result f x expected) (apply assert-result f more))))

(defn smoke-dist [dist]
  (assert-result dist
                 [1 2 3]   [1 2 3]
                 [1 2 1 2 3]   [1 2 3]
                 [] []
                 [1 1 1]   [1]
                 #{1 2 3}  (seq #{1 2 3})
                 (repeat 100 :x)  [:x]
                 (take 100 (cycle [:a :b]))  [:a :b]
                 (range 100)  (range 100)))

   
;; benchmark code from Tonksi
;; https://gist.github.com/tonsky/97dfe1f9c48eccafc983a49c7042fb21
;; in CLJ-
(require '[criterium.core :as c])

(defn format-time [estimate]
  (let [mean (first estimate)
        [factor unit] (c/scale-time mean)]
    (c/format-value mean factor unit)))


(defmacro race [body1 body2]
 `(let [_#        (assert (= ~body1 ~body2))
        results1# (c/quick-benchmark ~body1 {})
        results2# (c/quick-benchmark ~body2 {})
        percent#  (->> (/ (first (:mean results2#))
                          (first (:mean results1#)))
                       (* 100)
                       (- 100)
                       (int))]
    
    (println ~(pr-str body1) "\t"
             (format-time (:mean results1#)) "=>" (format-time (:mean results2#))
             (str "(" (- percent#) "%)"))))


(defn transient-distinct
  "Returns a lazy sequence of the elements of coll with duplicates removed.
  Returns a stateful transducer when no collection is provided."
  {:added "1.0"
   :static true}
  ([]
   (fn [rf]
     (let [seen ^clojure.lang.ITransientSet (transient #{})]
       (fn
         ([] (rf))
         ([result] (rf result))
         ([result input]
          (if (.contains seen input)
            result
            (do (conj! seen input)
                (rf result input))))))))
  ([coll]
   (let [step (fn step [xs seen]
                (lazy-seq
                  ((fn [[f :as xs] ^clojure.lang.ITransientSet seen]
                     (when-let [s (seq xs)]
                       (if (.contains seen f)
                         (recur (rest s) seen)
                         (cons f (step (rest s) (conj! seen f))))))
                   xs seen)))]
     (step coll (transient #{})))))


(defn zdist
  "Returns a lazy sequence of the elements of coll with duplicates removed.
  Returns a stateful transducer when no collection is provided."
  {:added "1.0"
   :static true}
  ([]
   (fn [rf]
     (let [seen ^clojure.lang.ITransientSet (transient #{})]
       (fn
         ([] (rf))
         ([result] (rf result))
         ([result input]
          (if (.contains seen input)
            result
            (do (conj! seen input)
                (rf result input))))))))
  ([coll]
   (let [step (fn step [xs seen]
                (lazy-seq
                  ((fn [xs ^clojure.lang.ITransientSet seen]
                     (when-let [s (seq xs)]
                       (let [f (first s)]
                       (if (.contains seen f)
                         (recur (rest s) seen)
                         (cons f (step (rest s) (conj! seen f)))))))
                   xs seen)))]
     (step coll (transient #{})))))


(defn ydist
  "Returns a lazy sequence of the elements of coll with duplicates removed.
  Returns a stateful transducer when no collection is provided."
  {:added "1.0"
   :static true}
  ([]
   (fn [rf]
     (let [seen (transient #{})]
       (fn
         ([] (rf))
         ([result] (rf result))
         ([result input]
          (if (contains? seen input)
            result
            (do (conj! seen input)
                (rf result input))))))))
  ([coll]
   (let [step (fn step [xs seen]
                (lazy-seq
                  ((fn [xs seen]
                     (when-let [s (seq xs)]
                       (let [f (first s)]
                       (if (contains? seen f)
                         (recur (rest s) seen)
                         (cons f (step (rest s) (conj! seen f)))))))
                   xs seen)))]
     (step coll (transient #{})))))





;; SEM hacked
(defn tonski-bench []
  (doseq [size [10 100 1000 10000]
          :let [coll (vec (repeatedly size #(rand-int 1000)))]]
    (println size "elements")

    (print "distinct vs dist ")
    (race (doall (distinct coll))
          (doall (dist coll)))

    (print "distinct vs zdist ")
    (race (doall (distinct coll))
          (doall (zdist coll)))

    (print"transient-distinct vs zdist ")    
    (race (doall (transient-distinct coll))
          (doall (zdist coll)))

    (print"ydist vs zdist ")    
    (race (doall (ydist coll))
          (doall (zdist coll)))

    #_
    (race (into [] (distinct) coll)
          (into [] (transient-distinct) coll))))


;; [old results]
;; 10 elements
;; (doall (distinct coll)) 	 5.773439 µs => 4.179092 µs (-27%)
;; (into [] (distinct) coll) 	 3.238236 µs => 1.943254 µs (-39%)
;; 
;; 100 elements
;; (doall (distinct coll)) 	 67.725764 µs => 42.129993 µs (-37%)
;; (into [] (distinct) coll) 	 35.702741 µs => 16.495947 µs (-53%)
;; 
;; 1000 elements
;; (doall (distinct coll)) 	 540.652739 µs => 399.053873 µs (-26%)
;; (into [] (distinct) coll) 	 301.423077 µs => 164.025500 µs (-45%)
;; 
;; 10000 elements
;; (doall (distinct coll)) 	 3.439137 ms => 3.058872 ms (-11%)
;; (into [] (distinct) coll) 	 1.437390 ms => 848.277178 µs (-40%)

 
