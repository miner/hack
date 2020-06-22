(ns miner.euniq)

;; https://gist.github.com/ericnormand/235696a7f7cca803f772a2d1bd1b90c3
;; Note that the uniques are supposed to be in the order of the original coll.  That forces
;; a bit of extra work.

;; best
(defn uniques [coll]
  (let [freqs (frequencies coll)]
    (sequence (filter (fn [x] (= (get freqs x) 1))) coll)))


;; not much faster
;; incorporate frequencies but skip persistent! to save time.
(defn uni4 [coll]
  (let [freqs (reduce (fn [counts x]
                        (assoc! counts x (inc (get counts x 0))))
                      (transient {})
                      coll)]
    (sequence (filter (fn [x] (= (freqs x) 1))) coll)))



(defn uni3 [coll]
  (let [freqs (frequencies coll)]
    (sequence (filter (fn [x] (= (freqs x) 1))) coll)))


(defn uniques2 [coll]
  (let [freqs (frequencies coll)]
    (filter (fn [x] (= (get freqs x) 1)) coll)))

;; fastest, not lazy at all
(defn uniques1 [coll]
  (let [freqs (frequencies coll)]
    (into [] (filter (fn [x] (= (get freqs x) 1))) coll)))


(defn uni [coll]
  (loop [onces [] mults #{} xs (seq coll)]
    (if xs
      (let [x (first xs)]
        (if (contains? mults x)
          (recur (into [] (remove #{x}) onces) mults (next xs))
          (recur (conj onces x) (conj mults x) (next xs))))
      onces)))
  
(defn smoke-un
  ([] (smoke-un uniques))
  ([uniques]
   (assert (= (uniques [1 2 3 4 5 6 1 2 3 5 6]) '(4)))
   (assert (= (uniques [:a :b :c :c]) '(:a :b)))
   (assert (= (uniques [1 2 3 1 2 3]) ()))
   ;; espensive test but shows that original ordering matters
   (assert (= (uniques (cons 19 (range 20))) (range 19)))
   true))
