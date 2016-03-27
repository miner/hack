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


;;; 02/01/16  14:22 by miner -- this was old, before transducers.  Might be worth a second
;;; look.

;; related CLJ-1764
;; partition-by on infinite input
;; would like more laziness on infinite input, but is that really necessary?


(defn core-partition-by [f coll]
  ;; original, without transducer code
  (lazy-seq
   (when-let [s (seq coll)]
     (let [fst (first s)
           fv (f fst)
           run (cons fst (take-while #(= fv (f %)) (next s)))]
       (cons run (core-partition-by f (seq (drop (count run) s))))))))


;; Will still have to count potentially infinite tail!
;; Correction:  it works because it doesn't have to count until it's really needed, which is
;; never for the last partition.
(defn proposed-partition-by [f coll]
  (lazy-seq
   (when-let [s (seq coll)]
     (let [fst (first s)
           fv (f fst)
           run (cons fst (take-while #(= fv (f %)) (next s)))]
       (cons run (proposed-partition-by f (lazy-seq (drop (count run) s))))))))



;; SEM: but drop is lazy anyway so seq/lazy-seq not needed.  Right ???  Wrong!!  Infinite
;; lazy tail will never stop on drop/count preparing for "next" partition
;; BAD
(defn my-partition-by [f coll]
  (lazy-seq
   (when-let [s (seq coll)]
     (let [fst (first s)
           fv (f fst)
           run (cons fst (take-while #(= fv (f %)) (next s)))]
       (cons run (my-partition-by f (drop (count run) s)))))))

;; BAD
(defn my2-partition-by [f coll]
  (lazy-seq
   (when-let [s (seq coll)]
     (let [fst (first s)
           fv (f fst)
           run (cons fst (take-while #(= fv (f %)) (next s)))]
       (cons run (lazy-seq (my-partition-by f (drop (count run) s))))))))


;; It's lazy enough that it doesn't have to be forced
(defn drop-seq [drops coll]
  (lazy-seq
   (if-let [ds (seq drops)]
     (when-let [cs (seq coll)]
       (drop-seq (next ds) (next cs)))
     coll)))



;; Seems a little faster than "proposed" but not a big win
(defn sem-partition-by [f coll]
  (lazy-seq
   (when-let [s (seq coll)]
     (let [fst (first s)
           fv (f fst)
           run (cons fst (take-while #(= fv (f %)) (next s)))]
       (cons run (sem-partition-by f (drop-seq run s)))))))

(defn sem2-partition-by [f coll]
   (when-let [s (seq coll)]
     (let [fst (first s)
           fv (f fst)
           run (cons fst (take-while #(= fv (f %)) (next s)))]
       (cons run (sem-partition-by f (drop-seq run s))))))



(defn ppby [f coll]
  (when-let [s (seq coll)]
    (let [fst (first s)
          fv (f fst)
          run (cons fst (take-while #(= fv (f %)) (next s)))]
      (cons run (lazy-seq (ppby f (drop (count run) s)))))))


;; Testing partitions and mapcatting back together, hoping to stress laziness

#_ (dotimes [_ 20] (time (count (mapcat identity (sep/sem-partition-by #(zero? (mod % 1000))
                                                                       (range 10000000))))))

(defn stress
  ([pby] (stress pby 5e6))
  ([pby num]
   (println (str pby) num)
   (dotimes [_ 20]
     (time (count (mapcat identity (pby #(zero? (mod % 100))
                                        (range num))))))))


(defn stress2
  ([pby] (stress pby 5e6))
  ([pby num]
   (println (str pby) num)
   (dotimes [_ 20]
     (time (count (reduce into (pby #(zero? (mod % 100))
                                    (range num))))))))

