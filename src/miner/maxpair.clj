(ns miner.maxpair)

;;; https://www.lvguowei.me/post/nested-for-loops-in-clojure/
;;; discusses porting nested loops into Clojure

;;; Problem: Given an array of numbers, find the biggest sum of any two numbers. The same
;;; item in array cannot be used twice.


;;; author's solution:
(defn max-pair
  [v]
  (loop [i 0
         j 1
         result 0]
    (if (= i (dec (count v)))
      result
      (let [new-result (max result (+ (nth v i) (nth v j)))]
        (if (= j (dec (count v)))
          (recur (inc i) (-> i inc inc) new-result)
          (recur i (inc j) new-result))))))

;;; Bug in max-pair for edge cases on [] or [1]

;;; my version, just reduce once and keep track of highest two
(defn max-pair2 [v]
  (reduce +
          (if (<= (count v) 2)
            v
            ;; a is always greater than b
            (reduce (fn [[a b :as ab] c]
                      (cond (> c a) [c a]
                            (> c b) [a c]
                            :else ab))
                    (let [v0 (v 0)
                          v1 (v 1)]
                      (if (> v1 v0)
                        [v1 v0]
                        [v0 v1]))
                    (subvec v 2)))))

(defonce sample (shuffle (range 1000)))





;; (quick-bench (max-pair sample))
;; Execution time mean : 99.124048 µs

;; (quick-bench (max-pair2 sample))
;; Execution time mean : 1.114158 µs



;;; shorter but not faster
(defn max-pair21 [v]
  (reduce +
          (if (<= (count v) 2)
            v
            ;; a is always greater than b
            (reduce (fn [[a b :as ab] c]
                      (cond (> c a) [c a]
                            (> c b) [a c]
                            :else ab))
                    (vec (sort (subvec v 0 2)))
                    (subvec v 2)))))




;; simple but slower than max-pair2
(defn max-pair3 [v]
  (reduce + (take 2 (sort #(compare %2 %) v))))

;; not faster
(defn max-pair4 [v]
  (loop [a (max (v 0) (v 1)) b (min (v 0) (v 1)) cs (drop 2 v)]
    (if-let [c (first cs)]
      (cond (> c a) (recur c a (rest cs))
            (> c b) (recur a c (rest cs))
            :else (recur a b (rest cs)))
      (+ a b))))

;; not faster
(defn max-pair5 [v]
  (transduce (drop 2)
             (fn ([[a b :as ab] c]
                  (cond (> c a) [c a]
                        (> c b) [a c]
                        :else ab))
               ([res] (reduce + res)))
             (let [v0 (v 0)
                   v1 (v 1)]
               (if (> v1 v0)
                 [v1 v0]
                 [v0 v1]))
             v))

;;; on par with fastest
(defn max-pair51 [v]
  (transduce conj
             (fn ([[a b :as ab] c]
                  (cond (nil? a) [c]
                        (nil? b) (if (> c a) [c a] [a c])
                        (> c a) [c a]
                        (> c b) [a c]
                        :else ab))
               ([res] (reduce + res)))
             nil
             v))


;;; on par with fastest
(defn max-pair52 [v]
  (transduce (map identity)
             (fn ([[a b :as ab] c]
                  (cond (nil? a) [c]
                        (nil? b) (if (> c a) [c a] [a c])
                        (> c a) [c a]
                        (> c b) [a c]
                        :else ab))
               ([res] (reduce + res)))
             nil
             v))

;;; not as fast.  Seems like transduce is not optimized for subvec
(defn max-pair53 [v]
  (transduce (map identity)
             (fn ([[a b :as ab] c]
                  (cond (> c a) [c a]
                        (> c b) [a c]
                        :else ab))
               ([res] (reduce + res)))
             (let [v0 (nth v 0 0)
                   v1 (nth v 1 0)]
               (if (> v1 v0)
                 [v1 v0]
                 [v0 v1]))
             (subvec v 2)))


;;; simplifying (map identity)

;;; starting with map transducer
;;; f = identity, but that can just be the arg

;;; I guess last arity is needed for (sequence xform col col2 col3)
;;; but that won't be called for singular identity
(def xidentity
  (fn [rf]
    (fn
      ([] (rf))
      ([result] (rf result))
      ([result input]
       (rf result input))
      ([result input & inputs]
       (apply rf result input inputs)))))


(defn max-pair54 [v]
  (transduce xidentity
             (fn ([[a b :as ab] c]
                  (cond (> c a) [c a]
                        (> c b) [a c]
                        :else ab))
               ([res] (reduce + res)))
             (let [v0 (nth v 0 0)
                   v1 (nth v 1 0)]
               (if (> v1 v0)
                 [v1 v0]
                 [v0 v1]))
             (subvec v 2)))




;;; faster than max-pair2 depending on sample
(defn max-pair6 [v]
  (reduce +
          (reduce (fn [[a b :as ab] c]
                    (cond (nil? a) [c]
                          (nil? b) (if (> c a) [c a] [a c])
                          (> c a) [c a]
                          (> c b) [a c]
                          :else ab))
                  nil
                  v)))


;; not quite as fast with transduce so don't bother
(defn max-pair61 [v]
  (transduce xidentity
             (fn ([[a b :as ab] c]
                  (cond (nil? a) [c]
                        (nil? b) (if (> c a) [c a] [a c])
                        (> c a) [c a]
                        (> c b) [a c]
                        :else ab))
               ([res] (reduce + res)))
             nil
             v))

;; slightly slower
(defn max-pair62 [v]
  (transduce (map identity)
             (fn ([[a b :as ab] c]
                  (cond (nil? a) [c]
                        (nil? b) (if (> c a) [c a] [a c])
                        (> c a) [c a]
                        (> c b) [a c]
                        :else ab))
               ([res] (reduce + res)))
             nil
             v))
