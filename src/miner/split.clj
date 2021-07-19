(ns miner.split)
;; Redundant evaluations in split-with
;; https://clojure.atlassian.net/browse/CLJ-2627

;;; SEM: this is a suggestion by Alex Miller.  But it doesn't address all potential laziness
;;; issues.  I wonder if a transducer would be a better approach???

(defn clj-split-with
  "Returns a vector of [(take-while pred coll) (drop-while pred coll)]"
  [pred coll]
  [(take-while pred coll) (drop-while pred coll)])

(defn alex-split-with
  "Like `split-with` but evaluates `pred` at most once on each element"
  [pred coll]
  (let [takes (take-while pred coll)
        drops (lazy-seq (drop (count takes) coll))]
    [takes drops]))

;; but count forces too soon.  Alex's version uses lazy-seq to protect
(defn sem-split-with
  "SEM: Like `split-with` but evaluates `pred` at most once on each element"
  [pred coll]
  (let [takes (sequence (take-while pred) coll)
        drops (sequence (drop (count takes)) coll)]
    [takes drops]))


;; BAD: Slower and probably less lazy.  Or maybe not -- looks better for some tests.  BUT
;; definitely less lazy.
(defn xsplit-with
  [pred coll]
  (transduce (take-while pred) (fn ([tks x] (conj tks x))
                                 ([tks] [tks (sequence (drop (count tks)) coll)]))
             []
             coll))


(defn slow [f]
  (fn [& args]
    (Thread/sleep 1)
    (apply f args)))

;; make a test that wins with laziness
;; make a test with expensive pred -- that's the original point.  If pred is fast, it's hard
;; to beat original code.  If pred is slow, maybe it doesn't matter anyway.
(defn smoke-splits [splitw]
  (assert (= (splitw (slow #(< % 50)) (range 100))
             [(range 50) (range 50 100)]))
  (assert (= (splitw (slow neg?) (range -100 2))
             [(range -100 0) (range 2)]))
  (let [[ts ds] (splitw (slow #(< % 50)) (range 100))]
    (assert (and (= (first ts) 0) (= (first ds) 50))))
  true)


  
