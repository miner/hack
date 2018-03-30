(ns miner.sortcount
  (:require [criterium.core :as c]
            [clojure.data.avl :as avl]))

;; https://stackoverflow.com/questions/49039792/how-to-implement-counting-sort-for-integers-in-clojure

;; Say I have an array of integers xs taking values from 0 to max, and I need to sort it in
;; O(n) time, so I can't just do (sort xs)

;; SEM: I doubt you can do better than `sort`, but let's see...

(def xs [1 2 1 4 1 5 1 8 7 7 7])

(def ys [36 12 92 57 7 29 52 23 23 52 7 76 69 39 65 58 60 14 5 33 60 31 6 38 92 47 86 48 55
         64 79 98 73 18 30 91 57 84 27 43 4 97 49 0 78 46 15 39 39 13 20 90 89 52 11 86 9 48
         96 40 47 20 61 39 98 50 41 61 93 41 42 44 59 72 37 99 89 81 29 95 81 69 63 36 47 42
         12 45 27 12 50 63 38 78 45 7 5 9 52 62])

(def zs [1 3 9 0 3 4 4 0 0 5 6 0 9 3 5 2 4 9 4 2 1 6 5 7 2 1 8 8 6 5 3 5 1 8 4 0 6 7 9 7 6 2
         4 6 0 5 3 3 5 1 4 9 6 5 3 1 4 7 1 3 9 3 1 6 2 3 3 6 1 4 1 7 6 7 9 3 7 8 6 7 7 7 0 4
         7 8 9 1 8 1 1 6 3 2 1 1 2 0 7 9])

(def zsorted (sort zs))

(def tks (take 10000 (cycle (range 100))))

(def tksorted (sort tks))


(defn counting-sort-ORIG [s]
  (if (empty? s)
    s
    (let [counts (reduce (fn [v e]
                           (update v e inc))
                         (vec (repeat (inc (apply max s)) 0))
                         s)]
      (apply concat (map-indexed #(repeat %2 %1) counts)))))

;; SEM modified to accept known maximum
(defn counting-sort
  ([s] (counting-sort s (apply max s)))
  ([s maximum]
  (if (empty? s)
    s
    (let [counts (reduce (fn [v e]
                           (update v e inc))
                         (vec (repeat (inc maximum) 0))
                         s)]
      (apply concat (map-indexed #(repeat %2 %1) counts))))))





(comment 
  (let [xs [1 2 1 4 1 5 1 8 7 7 7]
        fs (frequencies xs)
        max 10]
    (into [] 
          cat
          (for [i (range max) :when (contains? fs i)]
            (repeat (get fs i) i))))
  )

;; rewritten as function
;; dynamically finds mx which could be slow
(defn counting-sort2 [xs]
  (let [fs (frequencies xs)
        mx (apply max xs)]
  (into [] 
    cat
    (for [i (range (inc mx)) :when (contains? fs i)]
      (repeat (get fs i) i)))))



;; slowish
(defn csort-mapcat [xs]
  (mapcat identity (vals (reduce (fn [m x] (update m x conj x)) (sorted-map) xs))))

(defn csort1 [xs]
  (into [] cat (vals (reduce (fn [m x] (update m x conj x)) (sorted-map) xs))))

(defn csort2 [xs]
  (sequence cat (vals (reduce (fn [m x] (update m x conj x)) (sorted-map) xs))))

(defn csort-almost [xs]
  (reduce-kv (fn [r _ v] (into r v))
             []
             (reduce (fn [m x] (update m x conj x)) (sorted-map) xs)))



(defn csort-competitive [xs]
  (persistent!
   (reduce-kv (fn [r _ v] (reduce conj! r v))
              (transient [])
              (reduce (fn [m x] (update m x conj x)) (sorted-map) xs))))

;; still not fast enough, but pretty good with avl
(defn csort-avl [xs]
  (persistent!
   (reduce-kv (fn [r _ v] (reduce conj! r v))
              (transient [])
              (persistent! (reduce (fn [m x] (assoc! m x (conj (get m x) x))) (transient (avl/sorted-map)) xs)))))


(defn csort-better
  ([s] (if (empty? s) s (csort-better s (apply max s))))
  ([s maximum]
   (let [counts (persistent! (reduce (fn [tv e]
                                       (assoc! tv e (inc (get tv e))))
                                     (reduce conj! (transient []) (repeat (inc maximum) 0))
                                     s))]
     (into [] (comp (map-indexed #(repeat %2 %1)) cat)  counts))))


;; this is pretty good, but regular old `sort` is still better unless you need the
;; intermediate counts

(defn csorta
  ([xs] (if (empty? xs) xs (csorta xs (apply max xs))))
  ([xs maximum]
   (into []
         (comp (map-indexed #(repeat %2 %1)) cat)
         (reduce (fn [^longs ar ^long e]
                   (aset-long ar e (inc (aget ar e)))
                   ar)
                 (long-array (inc maximum))
                 xs))))

(def csort csorta)


;; slower than frequencies, but maybe better if you knew the coll was sorted???
(defn sfreq [xs]
  (let [ss (sort xs)]
    (dissoc 
     (reduce (fn [m x]
               (if (= x (::current m))
                 (update m x inc)
                 (assoc m x 1 ::current x)))
             {::current nil}
             ss)
     ::current)))

;; No, not faster even when ss is presorted
(defn presfreq [ss]
    (dissoc 
     (reduce (fn [m x]
               (if (= x (::current m))
                 (update m x inc)
                 (assoc m x 1 ::current x)))
             {::current nil}
             ss)
     ::current))

(defn smoke []
  (assert (= (sort xs) (csort xs)))
  true)


(defn testf
  ([sortf] (testf sortf tks (sort tks)))
  ([sortf xs result]
   (assert (= result (sortf xs)))
   true))

(defn ben
  ([] (ben tks))
  
  ([xs]
   (let [sorted (sort xs)]
   (println "** sort")
   (c/quick-bench (testf sort xs sorted))
   (println "** counting-sort")
   (c/quick-bench (testf counting-sort xs sorted))

   (println "** counting-sort known maximum")
   (let [maxi (apply max xs)
         cs #(counting-sort % maxi)]
     (c/quick-bench (testf cs xs sorted)))

   (println "** csort")
   (c/quick-bench (testf csort xs sorted))

   (println "** csort known maximum")
   (let [maxi (apply max xs)
         cs #(csort % maxi)]
     (c/quick-bench (testf cs xs sorted))))))


