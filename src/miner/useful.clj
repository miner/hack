(ns miner.useful)

;; SEM variations on the "Useful" library.
;; https://cljdoc.org/d/org.flatland/useful/0.11.6/doc/readme
;; https://github.com/clj-commons/useful

;; I copy the original function and then try some variations.  Not ready for primetime.


(defn slice
  "Divide coll into n approximately equal slices."
  [n coll]
  (loop [num n, slices [], items (vec coll)]
    (if (empty? items)
      slices
      (let [size (Math/ceil (/ (count items) num))]
        (recur (dec num) (conj slices (subvec items 0 size)) (subvec items size))))))

;; SEM: seems like `partition-all` but with number of partitions vs. size of each partition.
;; But `slice` distributes "extras" amoung the partitions rather than adding an extra partition.

(defn not-exactly-slice [n coll]
  (partition-all (quot (count coll) n) coll))

(defn test-size []
  (let [items (range 131)]
    (keep (fn [num]
            (let [size (Math/ceil (/ (count items) num))
                  qsize (quot (+ (count items) (dec num)) num)]
              (when-not (== size qsize)
                [num (count items) size qsize])))
          (range 1 100))))
      


(defn orig-size [num items] (Math/ceil (/ (count items) num)))

(defn sem-size1 [num items] (quot (+ (count items) (dec num)) num))

(defn bensize [fsize]
  (let [items (vec (range 131))]
    (reduce + 0 (map #(fsize % items) (range 1 21)))))


;; x is any num, d assumed to be int
;; long result
(defn ceil
  ([x] (if (int? x) x (long (Math/ceil x))))
  ([x d] (if (int? x)
           (quot (+ x (dec d)) d)
           (long (Math/ceil (/ x d))))))


(defn sem-size [num items] (ceil (count items) num))



(defn bsize []
  (let [items (vec (range 131))]
    (reduce + 0 (map (fn [num] (Math/ceil (/ (count items) num))) (range 1 21)))))


(defn bqsize []
  (let [items (vec (range 131))]
    (reduce + 0 (map (fn [num] (quot (+ (count items) (dec num)) num)) (range 1 21)))))



;; ----------------------------------------------------------------------

(defn alternates
  "Split coll into 'threads' subsequences (defaults to 2), feeding
  each alternately from the input sequence. Effectively the inverse of
  interleave:
  (alternates 3 (range 9))
  ;=> ((0 3 6) (1 4 7) (2 5 8))"
  ([coll] (alternates 2 coll))
  ([threads coll]
     (lazy-seq
      (when (seq coll)
        (apply map list (partition threads coll))))))

(defmacro lazy-loop
  "Provide a simplified version of lazy-seq to eliminate
  boilerplate. Arguments are as to the built-in (loop...recur),
  and (lazy-recur) will be defined for you. However, instead of doing
  actual tail recursion, lazy-recur trampolines through lazy-seq. In
  addition to enabling laziness, this means you can call lazy-recur
  when not in the tail position.
  Regular recurs are also supported, if they are in tail position and don't
  need any laziness."
  [bindings & body]
  (let [f 'lazy-recur
        [names values] (alternates bindings)
        blob-names (repeatedly (count names) gensym)]
    `(letfn [(~f [~@blob-names]
               (lazy-seq
                 (iter# ~@blob-names)))
             (iter# [~@names]
               ~@body)]
       (~f ~@values))))


(defn unfold
  "Traditionally unfold is the 'opposite of reduce': it turns a single
  seed value into a (possibly infinite) lazy sequence of output
  values.
  Next is a function that operates on a seed: it should
  return a pair, [value new-seed]; the value half of the pair is
  inserted into the resulting list, while the new seed is used to
  continue unfolding. Notably, the value is never passed as an
  argument to next. If nil is returned instead of a pair, the resulting
  sequence will terminate.
  (defn fibs []
    (unfold (fn [[a b]]
              [a [b (+ a b)]])
            [0 1]))"
  [next seed]
  (lazy-loop [seed seed]
    (when-let [[val seed] (next seed)]
      (cons val (lazy-recur seed)))))


;;; ----------------------------------------------------------------------

(defn fibs [n]
  (take n (unfold (fn [[a b]]
                    [a [b (+ a b)]])
                  [0 1])))


(defn sunfold-WORKS [step init cnt]
  (loop [res [init] i cnt]
    (if (zero? i)
      (pop res)
      (if-let [[val state] (step (peek res))]
        (recur (conj (pop res) val state) (dec i))
        (pop res)))))

(defn sunfold [step init cnt]
  (loop [res [init] i cnt]
    (if-let [[val state] (when (pos? i) (step (peek res)))]
      (recur (conj (pop res) val state) (dec i))
      (pop res))))



(defn fibs2 [n]
  (sunfold (fn [[a b]]
             [a [b (+ a b)]])
           [0 1]
           n))



;;  (alternates 3 (range 9))
;;=> ((0 3 6) (1 4 7) (2 5 8))"

(defn alts [n coll]
  (apply sequence (map list) (partition n coll)))

(defn alts2 [n coll]
  (apply sequence (map list) (partition n coll)))


;; inspired by "useful" library, which has a good, lazy version.
;;
;; However, I did't necessarily like the API.  My stuff is definitely not ready for
;; prime-time.




(defn vunfold [step init]
  (loop [res init]
    (let [res2 (step res)]
      (cond (not res2) res
            (reduced? res2) @res2
            :else (recur res2)))))


(defn nunfold [n step init]
  (loop [res init]
    (if (= (count res) n)
      res
      (let [res2 (step res)]
        (cond (not res2) res
              (reduced? res2) @res2
              :else (recur res2))))))


(defn tunfold [term? step init]
  (loop [res init]
    (if-let [res3 (term? res)]
      res3
      (let [res2 (step res)]
        (cond (not res2) res
              (reduced? res2) @res2
              :else (recur res2))))))

(defn vfib [n]
  (vunfold (fn [v] (when (< (count v) n) (conj v (+ (peek v) (peek (pop v))))))
           [0 1]))

(defn nfib [n]
  (nunfold n
           (fn [v] (conj v (+ (peek v) (peek (pop v)))))
           [0 1]))


(defn tfib [n]
  (tunfold (fn [v] (when (= (count v) n) v))
           (fn [v] (conj v (+ (peek v) (peek (pop v)))))
           [0 1]))

