(ns miner.separate)

;; ideas inspired by the mailing list discussion
;; Analog to Scheme's partition in Clojure?

;; Issue: walking seq twice -- matters if it's a slow lazy seq.  Rarely impoortant.
;; Issue: calling pred twice -- matters if it's an expensive pred.  More important.
;; Issue: reduce is eager, would prefer to stay lazy.
;; Chunking complicates the lazy evaluation.

;; Probably the simplest is best.

;; from contrib (seq?)
(defn separate [pred coll] 
  [(filter pred coll) (remove pred coll)])

;; in theory, lseparate might be faster if pred is very expensive
;; but watch out for chunking.  Also, the JVM might be smart enough to optimize the first
;; one so it doesn't have to call pred so much.
(defn lseparate [pred coll] 
  (let [pairs (map (fn [v] (if (pred v) [v nil] [nil v])) coll)] 
    [(keep first pairs) (keep second pairs)]))

;; Reduce is eager so not good for infinite coll
(defn rseparate [pred coll] 
  (reduce (fn [[succ fail] v] (if (pred v) [(conj succ v) fail] [succ (conj fail v)]))
          [[][]] 
          coll))

(defn keep-alt 
  ([bool-val-seq] (keep-alt bool-val-seq []))
  ([[test val & more] res]
     (cond test (recur more (conj res val))
           (seq more) (recur more res)
           :else (or (seq res) ()))))

(defn remove-alt 
  ([bool-val-seq] (remove-alt bool-val-seq []))
  ([[test val & more] res]
     (cond test (recur more res)
           (seq more) (recur more (conj res val))
           :else (or (seq res) ()))))

(defn iseparate [pred coll]
  (let [bs (map pred coll)
        bvs (interleave bs coll)]
    [(keep-alt bvs) (remove-alt bvs)]))



(defn nseparate [pred coll]
  (let [bs (mapcat #(if (pred %) [% nil] [nil %]) coll)]
    [(keep identity (take-nth 2 bs)) (keep identity (take-nth 2 (rest bs)))]))

(defn slow-pred [pred delay]
  (fn [x]
    (Thread/sleep delay)
    (pred x)))

(def slow-odd? (slow-pred odd? 100))

(defn third? [x]
  (zero? (mod x 3)))

(def slow-third? (slow-pred third? 100))

(defn slow-range [n] 
  (map (slow-pred identity 10) (range n)))



#_ (time (print (take 35 (first (separate slow-odd? (slow-range 100))))))


;; base on "outerleave" -- https://gist.github.com/alandipert/5827439
(defn outerleave [n coll]
  (->> (repeat coll)
       (map drop (range n))
       (map (partial take-nth n))))

;; maybe candidate for reducers?

(defn take-nth-off
  "Returns a lazy seq of every nth item in coll."
  ([n coll] (take-nth n coll))
  ([n offset coll] (take-nth n (drop offset coll))))

;; my version is a bit faster than outerleave
(defn separate-nth [n coll]
  (map #(take-nth-off n % coll) (range n)))
