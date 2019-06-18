(ns miner.remove
  (:require [criterium.core :as cc]))

;; https://purelyfunctional.tv/issues/purelyfunctional-tv-newsletter-320-tip-know-clojure-s-execution-semantics-for-better-repl-driven-development/


;; Clojure puzzle
;; (remove-at 3 [1 2 3 4 5 6])
;; => (1 2 3 5 6)
;; Make this robust. You'll have to make some hard design decisions like how to handle the
;; empty sequence, how to handle out-of-bounds n, and more. Bonus points for clarity and
;; efficiency. But the #1 priority is completeness and correctness. Please document your
;; choices in comments. Extra credit: write a separate version for sequences and for
;; vectors. The vector version should take and return vectors.



;; I wrote several implementations.  The `n` must be a fixed precision integer, and the
;; `coll` must be seqable (typically a list or a vector).  In general, laziness is a virtue
;; so I used lazy functions.  If n is negative or greater than the last index of the
;; collection, I just return the whole collection rather than throwing an error.
;; Internally, the vector case is handled specially using subvec, but still in a lazy manner.  
;; It seems like this might be useful as a transducer so I implemented that as my preferred
;; solution.



;; Classic seq style
(defn remove-at-coll [n coll]
  {:pre [(int? n) (seqable? coll)]}
  (concat (take n coll) (drop (inc n) coll)))

;; Specialized for vectors.  Lazy.  Returns whole vector if index is out of bounds.
(defn remove-at-vec [n v]
  {:pre [(int? n) (vector? v)]}
  (if (< -1 n (count v))
    (lazy-cat (subvec v 0 n) (subvec v (inc n)))
    v))

;; faster, but eager
(defn remove-at-vec-eager [n v]
  {:pre [(int? n) (vector? v)]}
  (if (< -1 n (count v))
    (into (subvec v 0 n) (subvec v (inc n)))
    v))


;; Transducer version
(defn remove-at
  "Returns a lazy sequence of the elments of the collection `coll` with the element at index
  `n` removed.  Returns a stateful transducer when no collection is provided."
  ([n]
   {:pre [(int? n)]}
   (fn [rf]
     (let [nv (volatile! n)]
       (fn
         ([] (rf))
         ([result] (rf result))
         ([result input]
          (let [n @nv]
            ;; n=0 on input to skip
            (cond (pos? n) (do (vswap! nv unchecked-dec) (rf result input))
                  (zero? n) (do (vreset! nv -1) result)
                  :else (rf result input))))))))

  ([n coll]
   {:pre [(int? n) (seqable? coll)]}
   (sequence (remove-at n) coll)))






;; if the n is not an appropriate index for this v, just return the whole v
;; faster, but eager
(defn remove-at-vec1 [n v]
  {:pre [(int? n) (vector? v)]}
  (if (< -1 n (count v))
    (into (subvec v 0 n) (subvec v (inc n)))
    v))


(defn remove-at-vec3 [n v]
  {:pre [(int? n) (vector? v)]}
  (if (< -1 n (count v))
    (sequence cat (list (subvec v 0 n) (subvec v (inc n))))
    v))

;; SEM not sure about into performance, depends on where the removal is, but other
;; approaches require more setup.  Also, how valuable is laziness?  Using `into` is eager.
;;
;; A potential downside is that when `coll` is a vector, it can't be garbage collected until
;; the result is realized, but that is true with most lazy functions.



(defn remove-at-SAVE
  "Returns a lazy sequence of the elments of the collection `coll` with the element at index
  `n` removed.  Returns a stateful transducer when no collection is provided."
  ([n]
   {:pre [(int? n)]}
   (if (neg? n)
     (fn [rf]
       (fn 
         ([] (rf))
         ([result] (rf result))
         ([result input] (rf result input))))
     (fn [rf]
       (let [nv (volatile! n)]
         (fn
           ([] (rf))
           ([result] (rf result))
           ([result input]
            (if-let [n @nv]
              (if (zero? n)
                (do (vreset! nv nil)
                    result)
                (do (vswap! nv dec)
                    (rf result input)))
              (rf result input))))))))
  ([n coll]
   {:pre [(int? n) (seqable? coll)]}
   (if (vector? coll)
     (if (< -1 n (count coll))
       (lazy-cat (subvec coll 0 n) (subvec coll (inc n)))
       coll)
     (concat (take n coll) (drop (inc n) coll)))))






(defn remove-at42
  "Returns a lazy sequence of the elments of the collection `coll` with the element at index
  `n` removed.  Returns a stateful transducer when no collection is provided."
  ([n]
   {:pre [(int? n)]}
   (fn [rf]
     (let [nv (volatile! n)]
       (fn
         ([] (rf))
         ([result] (rf result))
         ([result input]
          (let [n @nv]
            (if (neg? n)
              (rf result input)
              (do (vswap! nv unchecked-dec)
                  (if (pos? n) 
                    (rf result input)
                    ;; skip input when n=0
                    result)))))))))
  ([n coll]
   {:pre [(int? n) (seqable? coll)]}
   (sequence (remove-at42 n) coll)))


(defn remove-at4
  "Returns a lazy sequence of the elments of the collection `coll` with the element at index
  `n` removed.  Returns a stateful transducer when no collection is provided."
  ([n]
   {:pre [(int? n)]}
   (fn [rf]
     (let [nv (volatile! n)]
       (fn
         ([] (rf))
         ([result] (rf result))
         ([result input]
          (let [n @nv]
            (cond (pos? n) (do (vswap! nv unchecked-dec) (rf result input))
                  (neg? n) (rf result input)
                  ;; n=0, skip input
                  :else (do (vreset! nv -1) result))))))))
  ([n coll]
   {:pre [(int? n) (seqable? coll)]}
   (sequence (remove-at4 n) coll)))

(defn remove-at31
  "Returns a lazy sequence of the elments of the collection `coll` with the element at index
  `n` removed.  Returns a stateful transducer when no collection is provided."
  ([n]
   {:pre [(int? n)]}
   (fn [rf]
     (let [nv (volatile! n)]
       (fn
         ([] (rf))
         ([result] (rf result))
         ([result input]
          (let [n @nv]
            ;; n=0 on input to skip
            (cond (zero? n) (do (vreset! nv -1) result)
                  (pos? n) (do (vswap! nv unchecked-dec) (rf result input))
                  :else (rf result input))))))))

  ([n coll]
   {:pre [(int? n) (seqable? coll)]}
   (sequence (remove-at31 n) coll)))



(defn rem-at2 [n coll]
   (if (vector? coll)
     (if (< -1 n (count coll))
       (lazy-cat (subvec coll 0 n) (subvec coll (inc n)))
       coll)
     (concat (take n coll) (drop (inc n) coll))))


(defn rem-at [n coll]
  (concat (take n coll) (drop (inc n) coll)))

;; or faster for big input???
;; slightly slower, and somewhat more complicated
(defn remove-at21
  "Returns a lazy sequence of the elments of the collection `coll` with the element at index
  `n` removed.  Returns a stateful transducer when no collection is provided."
  ([n]
   {:pre [(int? n)]}
   (fn [rf]
     (let [nv (volatile! n)]
       (fn
         ([] (rf))
         ([result] (rf result))
         ([result input]
          (let [n @nv]
            (if (neg? n)
              (rf result input)
              (do (vswap! nv unchecked-dec)
                  (if (pos? n) 
                    (rf result input)
                    ;; skip input when n=0
                    result)))))))))
  ([n coll]
   {:pre [(int? n) (seqable? coll)]}
   (if (vector? coll)
     (if (< -1 n (count coll))
       (lazy-cat (subvec coll 0 n) (subvec coll (inc n)))
       coll)
     (concat (take n coll) (drop (inc n) coll)))))



(defn remove-at2
  "Returns a lazy sequence of the elments of the collection `coll` with the element at index
  `n` removed.  Returns a stateful transducer when no collection is provided."
  ([n]
   {:pre [(int? n)]}
   (fn [rf]
     (let [nv (volatile! n)]
       (fn
         ([] (rf))
         ([result] (rf result))
         ([result input]
          (let [n @nv]
            (if (neg? n)
              (rf result input)
              (do (vswap! nv unchecked-dec)
                  (if (zero? n)
                    result
                    (rf result input))))))))))
  ([n coll]
   {:pre [(int? n) (seqable? coll)]}
   (sequence (remove-at2 n) coll)))


#_ (def v1k (vec (range 1000)))

#_ (transduce (comp (remove-at 50) (take 100)) + 0  v1k)

#_ (quick-bench (transduce (comp (remove-at 50) (take 100)) + 0  v1k))
;; Execution time mean : 4.372689 µs


#_ (quick-bench (reduce + 0 (take 100 (remove-at 50 v1k))))
;; Execution time mean : 13.691972 µs

#_ (quick-bench (transduce (comp (remove-at 50) (take 100)) + 0  (vec (range 1000))))
;; Execution time mean : 15.277574 µs

#_ (quick-bench (transduce (comp (remove-at 50) (take 100)) + 0  (range 1000)))
;; Execution time mean : 4.470632 µs

#_ (quick-bench (reduce + 0 (take 100 (remove-at 50 (range 1000)))))
;; Execution time mean : 18.331344 µs




(defn smoke-rm
  ([] (smoke-rm remove-at))
  ([rmat]
     (assert (= (rmat 3 (range 10)) '(0 1 2 4 5 6 7 8 9)))
     (dotimes [n 10]
       (assert (= (rmat n (range 10)) (remove #(= n %) (range 10)))))
     (assert (= (rmat 11 (range 10)) (range 10)))
     (assert (= (rmat -1 (range 10)) (range 10)))
   (assert (= (rmat 100 ()) ()))
   true))

(let [cnt 1000
      r1k (range cnt)
      v1k (vec r1k)
      cnt10 (quot cnt 10)
      sum10 (reduce + 0 (take cnt10 r1k))
      sum (reduce + 0 r1k)]
  (defn smoke-xrm [xrmat]
    (dotimes [n cnt]
      (assert (= (transduce (xrmat n) + 0 r1k) (- sum n))))
    true)
  
  (defn smoke-xtrm [xrmat]
    (dotimes [n cnt]
      (assert (= (transduce (comp (xrmat n) (take cnt10)) + 0 r1k)
                 (- (reduce + 0 (take (if (< n cnt10) (inc cnt10) cnt10) r1k))
                    (if (< n cnt10) n 0)))
              (str "xtrm failed " n)))
    true)
  )
  


(defn smoke-vrm
  ([] (smoke-vrm remove-at-vec))
  ([rmat]
   (let [v10 (vec (range 10))]
   (assert (= (rmat 3 v10) [0 1 2 4 5 6 7 8 9]))
   (dotimes [n 10]
     (assert (= (rmat n v10) (vec (remove #(= n %) (range 10)))) (str "Error at " n)))
   (assert (= (rmat 11 v10) v10))
   (assert (= (rmat -1 v10) v10))
   (assert (= (rmat 100 []) [])))
   true))


(defn ben [& rms]
  (println "\nStandard ---------")
  (doseq [rm rms]
    (println)
    (println (str rm))
    (cc/quick-bench (smoke-rm rm)))
  (println "\nTransducer ---------")
  (doseq [rm rms]
    (println)
    (println (str rm))
    (cc/quick-bench (smoke-xrm rm)))
  (println "\nTake with transducer ---------")
  (doseq [rm rms]
    (println)
    (println (str rm))
    (cc/quick-bench (smoke-xtrm rm)))
  )



  


(defn cben [& rms]
  (println "\nStandard ---------")
  (doseq [rm rms]
    (println)
    (println (str rm))
    (cc/quick-bench (smoke-rm rm))))

(defn vben [& rms]
  (println "\nStandard ---------")
  (doseq [rm rms]
    (println)
    (println (str rm))
    (cc/quick-bench (smoke-vrm rm))))

