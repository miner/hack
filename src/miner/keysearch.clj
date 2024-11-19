(ns miner.keysearch)

;;; https://ask.clojure.org/index.php/14231/gather-all-keys-values-nested-that-satisfies-some-condition


;;; Asking about how to search nested maps.  In this example, keys "k23" and "k24" are the targets.

(defn j-h5 [s mymap]
  (let [first-k (first (first s))
        first-v (second (first s)) ]
    (cond 
     (empty? s) mymap
     (or (= "k23" first-k) (= "k24" first-k))
         (recur (rest s) (assoc mymap first-k first-v))
     (map? first-v) (recur first-v mymap)
     :else (recur (rest s) mymap) 
     )))




(def j {"k1" "v1"
        "k2" {"k21" "v21" "k22" "v22" "k23" {"k231" "v231"} "k24" "v24"}
        "k3" "v3"})

(def jj {"k1" "v1"
        "k2" {"k21" "v21" "k22" {"k221" {"k23" "v221"}} "k23" {"k231" "v231" "k24" "v24"}}
        "k3" "v3"})

;;; FIXME -- better nested map with kws

#_ (j-h5 j {})
; expected output: {"k23" {"k231" "v231"} "k24" "v24"}


;;; SEM comments.  Don't need to see mymap.  Should be a local.  Key test should be a
;;; function, not hard-wired into function.  Note: a Clojure set acts as a funtion to test
;;; membership so that would work well.

;;; returns vector r, not a map, for result
;;; fastest for simple tests, probably not so great for deeply nested input
;;; explicit recursion might stack overflow.
;;; do you care about order of result?  breadth/depth?
;;; fastest on small examples, but a lazy approach would probably be better for big inputs.
(defn jhr [kpred? m]
  (reduce-kv (fn [r k v]
               (cond-> r
                   (kpred? k) (conj [k v])
                   (map? v) (into (jhrk kpred? v))))
             []
             m))


(defn jhrc [kpred? m]
  (reduce-kv (fn [r k v]
               (let [r (if (kpred? k) (conj r [k v]) r)]
                 (if (map? v) (concat (jhrk kpred? v) r) r)))
             []
             m))


;;; suggested answer.  tree-seq does a depth-first search
;;; changed to return vector, use mapcat.
;;; always descend map values even if key matched.
;;; staying lazy with sequence.
;;; probably best Clojure approach to be lazy, but not as fast as eager reduce or loop

(defn zjh [ks mp]
  (sequence (comp (filter map?)
                  (mapcat (fn [m] (seq (select-keys m ks)))))
            (tree-seq map? vals mp)))

;;; faster to filter in children fn, but not as elegant, and still much slower than eager
;;; approaches

(defn zjh2 [ks mp]
  (sequence (mapcat (fn [m] (seq (select-keys m ks))))
            (tree-seq map? #(filter map? (vals %)) mp)))



;;; Loop approach not as nice.
(defn jhl [kpred? smap]
  (loop [s (seq smap) res []]
    (if-let [[k v :as entry] (first s)]
      (recur (if (map? v)
               (into (rest s) (seq v))
               (rest s))
             (if (kpred? k) (conj res entry) res))
      res)))



(defn k234? [k]
  (or (= k "k23") (= k "k24")))

