(ns miner.grouping)

(def foos [[:a "foo"]  [:b "bar"]  [:a "baz"]])

;;  (reduce (fn [res [k v]] (update res k (fnil conj []) (clojure.string/upper-case v))) {} foos)

;; Ooops, misread the original posting and got the semantics of the grouping-fn wrong.
;; Basically, should extract key as in my second try.  Unfortunately, I posted the first
;; one.  But it's not hard to derive the right answer.

(defn mapping-group-by1 [grouping-fn mapping-fn coll]
  (reduce (fn [res [k v]] (update res k grouping-fn (mapping-fn v)))
          {}
          coll))

(defn smoke-test1 []
  (= (mapping-group-by1 (fnil conj []) clojure.string/upper-case foos)
     {:a ["FOO" "BAZ"] :b ["BAR"]}))


;; More like what was desired.  If your values don't have to be vectors, you can use conj
;; without the fnil

(defn mapping-group-by [grouping-fn mapping-fn coll]
  (reduce (fn [res item] (update res (grouping-fn item) (fnil conj []) (mapping-fn item)))
          {}
          coll))

(defn smoke-test []
  (= (mapping-group-by first (comp clojure.string/upper-case second) foos)
     {:a ["FOO" "BAZ"] :b ["BAR"]}))

;;----------------------------------------------------------------------
;; ONLY SEMI-RELATED TO ABOVE stuff
;;----------------------------------------------------------------------

;; Re Alex Miller talk at Philly ETE 2016
;; https://www.youtube.com/watch?v=V9b1ykFuE_c&spfreload=10#t=1445.463542557

;; His most-commits function does a group-by, then sort-by then last
;; I think it could be better.  Don't use last, sort opposite sense and take first.
;; But could instead use max-key without sorting at all.
;; If keyfn is expensive, could pre-compute in separate map and then max-key on constant
;; results.

;; Find his repo and try my ideas


;; use  accumulating-fn = conj if you don't care about the order of the items
(defn group-by-kv
  "Like `group-by` crossed with `reduce`.  `kf` returns a key for each item of
  coll. Optional `vf` returns transformed value for each item (default `identity`). Optional
  `accf` accumulates result for k, taking two arguments, the current accumulation and the
  additional value.  The default `accf` works like the standard `group-by`, accumulating
  results into a vector in order of coll.  Returns a map of accumulated results."
  ([kf coll] (group-by kf coll))
  ([kf vf coll] (group-by-kv kf vf (fnil conj []) coll))
  ([kf vf accf coll]
   (persistent!
    (reduce
     (fn [ret x]
       (let [k (kf x)]
         (assoc! ret k (accf (get ret k) (vf x)))))
     (transient {}) coll))))

;; pretty but maybe not as fast as using transients
(defn group-by-kv1
  ([key-fn coll] (group-by key-fn coll))
  ([key-fn val-fn coll] (group-by-kv1 key-fn val-fn (fnil conj []) coll))
  ([key-fn val-fn accumulating-fn coll]
   (reduce (fn [res item] (update res (key-fn item) accumulating-fn (val-fn item)))
           {}
           coll)))


;; assumes vals are numbers, like result from frequencies
(defn find-max [freqs]
  (reduce (fn [res kv] (if (> (val kv) (val res)) kv res)) freqs))


(def commits [{:committer :steve :id 123} {:committer :steve :id 456}
              {:committer :john :id 234} {:committer :steve :id 156}
              {:committer :bill :id 567} {:committer :john :id 765}])

(def big-commits (take 1000 (cycle commits)))

(defn inc2 [n _]
  (if n (inc n) 1))

(defn inc2n [n _]
  (if (nil? n) 1 (inc n)))

;; timings with criterium: (bench (most-commits big-commits))
;; 44.097192 µs
(defn most-commits [commits]
  (key (find-max (group-by-kv :committer identity (fn [r _] (if r (inc r) 1)) commits))))

;; very slightly slower
(defn most-commits0 [commits]
  (key (find-max (group-by-kv :committer identity (fn [r _] (inc (or r 0))) commits))))

;; slightly slower with named fn inc2
(defn most-commits00 [commits]
  (key (find-max (group-by-kv :committer identity inc2 commits))))

;; 54.961734 µs -- any? was slower than identity
(defn most-commits1 [commits]
  (key (find-max (group-by-kv :committer any? (fn [r v] (if r (inc r) 1)) commits))))

;; 69.815793 µs
(defn most-commits-miller [commits]
  (key (last (sort-by (comp count second) (group-by :committer commits)))))

;; 73.142483 µs
(defn most-commits-m2 [commits]
  (key (first (sort-by (comp - count second) (group-by :committer commits)))))


;; could sort commits by :committer first then partition-by :committer
;; slow: 348.561099 µs
(defn most-commits3 [commits]
  (:committer (first (apply max-key count
                            (partition-by :committer (sort-by :committer commits))))))



;; want some sort of "step f" transducer
;; consumes [state item] (f state item) returns new state (like reducer function)
;; transducer would be stateful
;; returns sequence of states
;; hmmm, needs more thought

;; 105.406918 µs
(defn most-commits4 [commits]
  (key (find-max (frequencies (map :committer commits)))))

;; 255.373921 µs
(defn most-commits5 [commits]
  (first (apply max-key count
                (partition-by identity (sort (map :committer commits))))))

;; I accidentally re-invented frequencies and ended up with another version of
;; most-commits4.  With the same performance!








;; Dubious general utility, but in some isolated cases, these might be handy.

;; like fnil but returned fn takes any number of args, ignoring all but first
(defn fnil1 [f x]
  (fn [a & _]
    (f (if (nil? a) x a))))

;; Converts f to a function that takes exactly N args, ignoring extras.
(defn fignore [f ^long n]
  (case n
    0 (fn [& _] (f))
    1 (fn [a & _] (f a))
    2 (fn [a b & _] (f a b))
    3 (fn [a b c & _] (f a b c))
    (fn [& args]
      (let [actuals (take n args)]
        (when (not= n (count actuals))
          (throw (clojure.lang.ArityException. (count actuals) (str f))))
      (apply f actuals)))))

;; like fnil but actual call only uses as many args are given (required), others ignored
(defn fnils
  ([f x]   (fn [a & _]
             (f (if (nil? a) x a))))
  ([f x y]   (fn [a b & _]
               (f (if (nil? a) x a) (if (nil? b) y b))))

  ([f x y z]   (fn [a b c & _]
               (f (if (nil? a) x a) (if (nil? b) y b) (if (nil? c) z c))))

  ([f x y z & subs]   (fn [a b c & ds]
                        (when (> (count subs) (count ds))
                          (throw (clojure.lang.ArityException. (+ 3 (count ds)) (str f))))
                        (apply f (if (nil? a) x a) (if (nil? b) y b) (if (nil? c) z c)
                               (map #(if (nil? %) %2 %1) (concat ds (repeat nil)) subs)))))

;; review ArityException string should be consistent for 1 vs 4 args
