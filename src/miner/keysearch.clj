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
                   (map? v) (into (jhr kpred? v))))
             []
             m))

;;; FASTEST but not always???
;;; for big input concat is faster -- I guess it's better than copying into
(defn jhrc [kpred? m]
  (reduce-kv (fn [r k v]
               (let [r (if (kpred? k) (conj r [k v]) r)]
                 (if (map? v) (concat (jhrc kpred? v) r) r)))
             []
             m))


;;; Inspired by suggested answer.  tree-seq does a depth-first search
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


(comment
(def ag [:a :b :c :d :e :f :g])
(def ag1 (zipmap ag (range)))
(def ag2 (reduce (fn [m k] (assoc m k ag1)) {} ag))
(def ag3 (reduce (fn [m k] (assoc m k ag2)) {} ag))
)

(def nnn
  {:a
   {:a {:a 0, :b 1, :c 2, :d 3, :e 4, :f 5, :g 6},
    :b {:a 0, :b 1, :c 2, :d 3, :e 4, :f 5, :g 6},
    :c {:a 0, :b 1, :c 2, :d 3, :e 4, :f 5, :g 6},
    :d {:a 0, :b 1, :c 2, :d 3, :e 4, :f 5, :g 6},
    :e {:a 0, :b 1, :c 2, :d 3, :e 4, :f 5, :g 6},
    :f {:a 0, :b 1, :c 2, :d 3, :e 4, :f 5, :g 6},
    :g {:a 0, :b 1, :c 2, :d 3, :e 4, :f 5, :g 6}},
   :b
   {:a {:a 0, :b 1, :c 2, :d 3, :e 4, :f 5, :g 6},
    :b {:a 0, :b 1, :c 2, :d 3, :e 4, :f 5, :g 6},
    :c {:a 0, :b 1, :c 2, :d 3, :e 4, :f 5, :g 6},
    :d {:a 0, :b 1, :c 2, :d 3, :e 4, :f 5, :g 6 :k 1},
    :e {:a 0, :b 1, :c 2, :d 3, :e 4, :f 5, :g 6},
    :f {:a 0, :b 1, :c 2, :d 3, :e 4, :f 5, :g 6},
    :g {:a 0, :b 1, :c 2, :d 3, :e 4, :f 5, :g 6}},
   :c
   {:a {:a 0, :b 1, :c 2, :d 3, :e 4, :f 5, :g 6},
    :b {:a 0, :b 1, :c 2, :d 3, :e 4, :f 5, :g 6},
    :c {:a 0, :b 1, :c 2, :d 3, :e 4, :f 5, :g 6},
    :d {:a 0, :b 1, :c 2, :d 3, :e 4, :f 5, :g 6},
    :e {:a 0, :b 1, :c 2, :d 3, :e 4, :f 5, :g 6},
    :f {:a 0, :b 1, :c 2, :d 3, :e 4, :f 5, :g 6 :k 2},
    :g {:a 0, :b 1, :c 2, :d 3, :e 4, :f 5, :g 6}},
   :k 0
   :d
   {:a {:a 0, :b 1, :c 2, :d 3, :e 4, :f 5, :g 6},
    :k {:a 0, :b 1, :c 2, :d 3, :e 4, :f 5, :g 6 :k 3},
    :c {:a 0, :b 1, :c 2, :d 3, :e 4, :f 5, :g 6},
    :d {:a 0, :b 1, :c 2, :d 3, :e 4, :f 5, :g 6},
    :e {:a 0, :b 1, :c 2, :d 3, :e 4, :f 5, :g 6},
    :f {:a 0, :b 1, :c 2, :d 3, :e 4, :f 5, :g 6},
    :g {:a 0, :b 1, :c 2, :d 3, :e 4, :f 5, :g 6}},
   :e
   {:a {:a 0, :b 1, :c 2, :d 3, :e 4, :f 5, :g 6},
    :b {:a 0, :b 1, :c 2, :d 3, :e 4, :f 5, :g 6},
    :c {:a 0, :b 1, :c 2, :d 3, :e 4, :f 5, :g 6},
    :d {:a 0, :b 1, :c 2, :d 3, :e 4, :f 5, :g 6},
    :e {:a 0, :b 1, :c 2, :d 3, :e 4, :f 5, :g 6},
    :f {:a 0, :b 1, :c 2, :d 3, :e 4, :f 5, :g 6},
    :g {:a 0, :b 1, :c 2, :d 3, :e 4, :f 5, :g 6}},
   :f
   {:a {:a 0, :b 1, :c 2, :d 3, :e 4, :f 5, :g 6},
    :b {:a 0, :b 1, :c 2, :d 3, :e 4, :f 5, :g 6},
    :c {:a 0, :b 1, :c 2, :d 3, :e 4, :f 5, :g 6},
    :d {:a 0, :b 1, :c 2, :d 3, :e 4, :f 5, :g 6},
    :e {:a 0, :b 1, :c 2, :d 3, :e 4, :f 5, :g 6},
    :f {:a 0, :b 1, :c 2, :d 3, :e 4, :f 5, :g 6},
    :g {:a 0, :b 1, :c 2, :d 3, :e 4, :f 5, :g 6}},
   :g
   {:k 4
    :a {:a 0, :b 1, :c 2, :d 3, :e 4, :f 5, :g 6},
    :b {:a 0, :b 1, :c 2, :d 3, :e 4, :f 5, :g 6},
    :c {:a 0, :b 1, :c 2, :d 3, :e 4, :f 5, :g 6},
    :d {:a 0, :b 1, :c 2, :d 3, :e 4, :f 5, :g 6},
    :e {:a 0, :b 1, :c 2, :d 3, :e 4, :f 5, :g 6},
    :f {:a 0, :b 1, :c 2, :d 3, :e 4, :f 5, :g 6},
    :g {:a 0, :b 1, :c 2, :d 3, :e 4, :f 5, :g 6}}})
