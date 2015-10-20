(ns miner.submap
  (:require [clojure.core.reducers :as r])
  (:require [clojure.test :refer (deftest are)]))


;; gist.github.com/3554952 -- just miner1-sub= was posted
(defn miner1-sub=
  "Like = for most things, but maps compare recursively by only the keys in a, so it returns true
if a is a 'submap' of b."
  [a b]
  (if (and (map? a) (map? b))
    (reduce-kv (fn [result k v] (and result (miner1-sub= v (get b k)))) true a)
    (= a b)))

(defn miner2-sub=
  "Like = for most things, but maps compare recursively by only the keys in a, so it returns true
if a is a 'submap' of b."
  [a b]
  (if (and (map? a) (map? b))
    (reduce-kv (fn [result k v] (or (and result (miner2-sub= v (get b k))) (reduced false)))
               true a)
    (= a b)))


(defn miner3-sub=
  "Like = for most things, but maps compare recursively by only the keys in a, so it returns true
if a is a 'submap' of b."
  [a b]
  (if (and (map? a) (map? b))
    (r/reduce (fn [result k v] (or (and (miner3-sub= v (get b k)) result) (reduced false)))
              true a)
    (= a b)))

(defn miner4-sub=
  "Like = for most things, but maps compare recursively by only the keys in a, so it returns true
if a is a 'submap' of b."
  [a b]
  (if (and (map? a) (map? b))
    (r/reduce (fn [result k v] (if result (miner4-sub= v (get b k)) (reduced false)))
              true a)
    (= a b)))


(defn andfn
  ([] true)
  ([x] x)
  ([x y] (and x y)) ([x y & zs] (and x y (apply andfn zs))))


(defn miner5-sub=
  "Like = for most things, but maps compare recursively by only the keys in a, so it returns true
if a is a 'submap' of b."
  [a b]
  (if (and (map? a) (map? b))
    (let [ks (keys a)
          b2 (select-keys b ks)]
      (map (fn [k] (miner5-sub= (get a k) (get b k))) ks)

    (r/reduce (fn [result k v] (if result (miner5-sub= v (get b k)) (reduced false)))
              true a)
    (= a b)))



;;; idea use reducers


;; https://gist.github.com/3552458
(defn original-miller-sub=
  "Checks that all keys and vals in m1 are in m2 (but m2 can have extra stuff)"
  [o1 o2]
  (if (and (map? o1) (map? o2))
    (every? identity (map (fn [[k v]] (original-miller-sub= v (k o2))) o1))
    (= o1 o2)))

(defn miller-sub=
  "Checks that all keys and vals in m1 are in m2 (but m2 can have extra stuff)"
  [o1 o2]
  (if (and (map? o1) (map? o2))
    (every? identity (map (fn [[k v]] (miller-sub= v (get o2 k))) o1))
    (= o1 o2)))

;; SEM comments: don't assume kw k, should use get instead
;; reduce-kv probably faster
;; 1.5 reducers could be useful, also has (reduced) to short circuit

(def sub= miner4-sub=)

;; https://twitter.com/puredanger/status/241282082268143619
(deftest test-sub=
  (let [hhh (zipmap (range 1000) (range 1 1001))
        hnest (zipmap (range 1000) (repeat hhh))
        hnest1 (assoc hnest 1001 1002)
        hnestx (assoc-in hnest [500 500] :fail)]
    (are [expected left right] (= expected (sub= left right))
         ;; prim
         true 1 1
         false 1 2
         ;; maps
         true {} {:a 1}
         false {:a 1} {}
         true {:a 1} {:a 1}
         false {:a 1} {:a 2}
         true {:a 1} {:a 1 :b 2}
         ;; non-kw keys
         true {1 10 2 20} {1 10 2 20 3 30}
         ;; nested maps
         true {} {:a {:b 1}}
         false {:a 1} {:b {:c 2}}
         true {:a {:b 1}} {:a {:b 1}}
         true {:a {:b 1}} {:a {:b 1 :c 2}}
         true {:a {}} {:a {:b 1}}
         false {:a {:b 1}} {:a 1}
         false {:a {:b 1}} {:a {:b 2}}
         true {:a {:b 1}} {:a {:b 1 :c 2}}
         ;; deeply nested
         true hnest hnest1
         false hnest1 hnest
         false hnestx hnest)))

