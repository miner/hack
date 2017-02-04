(ns miner.associn
  (:require [clojure.test :refer :all]))


;; http://dev.clojure.org/jira/browse/CLJ-1771

(defn assoc-in+
  "Associates value(s) in a nested associative structure, where ks is a
   sequence of keys and v is the new value and returns a new nested structure.
   If any levels do not exist, hash-maps will be created."
   {:added "1.0"
    :static true}
  ([m [k & ks] v]
   (if ks
     (assoc m k (assoc-in (get m k) ks v))
     (assoc m k v)))
  ([m ks v & kvs]
   (let [ret (assoc-in m ks v)]
     (if kvs
       (if (next kvs)
         (recur ret (first kvs) (second kvs) (nnext kvs))
         (throw (IllegalArgumentException.
                 "assoc-in+ expects an even number of arguments after map, found odd number")))
       ret))))


(deftest test-assoc-in
  (are [x y] (= x y)
    [{3 :value}] (assoc-in+ [] [0 3] :value)
    {:k1 {:k2 :value}} (assoc-in+ {} [:k1 :k2] :value)
    {:k1 {nil {:k3 :value}}} (assoc-in+ {} [:k1 nil :k3] :value)
    {:k1 :v1 :k2 {:k3 :v2}} (assoc-in+ {} [:k1] :v1 [:k2 :k3] :v2))
  (is (thrown? Exception (assoc-in+ {} :not-a-seq :value)))
  (is (thrown? IllegalArgumentException (assoc-in+ {} [:k1] :v1 [:k2]))))

(defn tduce2
  [f init coll]
  (transduce (partition 2) (fn ([r] r) ([r [a b]] (f r a b))) init coll))

(defn ass-in
  ([m ks v] (assoc-in m ks v))
  ([m ks v & kvs] (tduce2 (partition 2) (fn [m [ks v]] (assoc-in m ks v)) (assoc-in m ks v) kvs)))

(deftest test-ass-in
  (let [assoc-in+ ass-in]
    (are [x y] (= x y)
      [{3 :value}] (assoc-in+ [] [0 3] :value)
      {:k1 {:k2 :value}} (assoc-in+ {} [:k1 :k2] :value)
      {:k1 {nil {:k3 :value}}} (assoc-in+ {} [:k1 nil :k3] :value)
      {:k1 :v1 :k2 {:k3 :v2}} (assoc-in+ {} [:k1] :v1 [:k2 :k3] :v2))
    (is (thrown? Exception (assoc-in+ {} :not-a-seq :value)))
    (is (thrown? IllegalArgumentException (assoc-in+ {} [:k1] :v1 [:k2])))))

