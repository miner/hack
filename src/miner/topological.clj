(ns miner.topological)

;;; https://blog.exupero.org/topological-sort-in-clojure/

;;; https://en.wikipedia.org/wiki/Topological_sorting
;;;
;;; a topological sort or topological ordering of a directed graph is a linear ordering of
;;; its vertices such that for every directed edge uv from vertex u to vertex v, u comes
;;; before v in the ordering

;;; Most of these solutions are based on Kahn's algorithm.    My version is sem-topo which is
;;; significantly faster due to slightly rearranged code.  There is another way of doing it
;;; using depth first search, also implemented below, which is much faster on large graphs.

;;; Note that there are often multiple acceptable topological sorts so to be careful, you
;;; have to check the answers rather than just comparing to one "correct" result.  All the
;;; implementations below seem to agree with the ordering for my tests so I'm cheating on
;;; the check.

(defn topological? [graph topo-sorted]
  (boolean
   (reduce (fn [seen x]
             (if (and (not (seen x))
                      (every? seen (get graph x)))
               (conj seen x)
               (reduced false)))
           #{}
           topo-sorted)))

;;; from Rosetta Code
(defn topological-sort [graph]
  (when (seq graph)
    (when-let [depless (seq (keep (fn [[k v]]
                                    (when (empty? v) k))
                                  graph))]
      (concat depless
              (topological-sort
               (into {}
                     (map (fn [[k v]]
                            [k (apply disj v depless)]))
                     (apply dissoc graph depless)))))))

;;; The examples are dependecy graphs.  For example, if A depends on B, C and D, it should
;;; not come before any of them in the result of the topological sort.  There may be more
;;; than one acceptable answer.

(def ggg {:a #{:b :c :d}
          :b #{:c :d}
          :c #{:d :e}
          :d #{}
          :e #{:d}})

;;; => [:d :e :c :b :a]

(def circ {:a #{:b}
           :b #{:c}
           :c #{:a}})

;;; => nil (failure)

;;; big example
(def bbb (reduce (fn [m i] (assoc m i (into #{100} (range i)))) {100 #{}} (range 100)))

;;; big and bad
(def xxx (assoc bbb 100 #{50}))

(defn test-topo [topological-sort]
  (assert (= (topological-sort ggg) '(:d :e :c :b :a)))
  (assert (nil? (topological-sort circ)))
  (assert (= (topological-sort bbb) (conj (range 100) 100)))
  (assert (nil? (topological-sort xxx)))
  true)

;;; just small examples
(defn test-topo-quick [topological-sort]
  (assert (= (topological-sort ggg) '(:d :e :c :b :a)))
  (assert (nil? (topological-sort circ)))
  true)

;; exupero's version
(defn topo-sort [graph]
  (when (seq graph)
    (when-let [ks (seq (keep (fn [[k v]] (when (empty? v) k)) graph))]
      (concat ks
              (topo-sort
               (into {}
                     (map (fn [[k v]] [k (apply disj v ks)]))
                     (apply dissoc graph ks)))))))


;; SAVE
(defn sem-topological-sort [graph]
  (loop [res [] g graph]
    (if (empty? g)
      res
      (let [ks (reduce-kv (fn [res k v] (if (seq v) res (conj res k))) [] g)
            m (update-vals (reduce dissoc g ks) (fn [v] (reduce disj v ks)))]
        (when (seq ks)
          (recur (into res ks) m))))))


;;; almost Fastest and my favorite
(defn sem-topo [graph]
  (loop [tks [] g graph]
    (if (empty? g)
      tks
      (when-let [ks (reduce-kv (fn [ks k v] (if (seq v) ks (conj ks k))) nil g)]
        (recur (into tks ks)
               (update-vals (reduce dissoc g ks) #(reduce disj % ks)))))))



;;; Based on my literal translation of Kahn's algo.
;;; Much faster to use vector S instead of set.  Fastest version now.
(defn kahn-topo [graph]
  (let [conj-k-when-empty (fn [ks k v] (if (seq v) ks (conj ks k)))]
    (loop [s (reduce-kv conj-k-when-empty [] graph)
           tks []
           g graph]
      (if-let [n (peek s)]
        (let [g (update-vals (dissoc g n) #(disj % n))]
          (recur (reduce-kv conj-k-when-empty (pop s) g)
                 (conj tks n)
                 g))
        (when (empty? g)
          tks)))))







;;; Slower to build up intermediate ks as a set
(defn sem-topo3 [graph]
  (loop [tks [] g graph]
    (if (empty? g)
      tks
      (let [ks (reduce-kv (fn [ks k v] (if (seq v) ks (conj ks k))) #{} g)]
        (when (seq ks)
          (recur (into tks ks)
                 (update-vals (reduce dissoc g ks) #(clojure.set/difference % ks))))))))


;; m is "marked" map, perm true, temp false, unmarked if not present
;; special key ::tks has the vector of nodes in topo order

(defn df-visit1 [g m n]
  (cond (nil? m) nil
        (true? (get m n)) m
        (false? (get m n)) nil
        :else (let [m2 (assoc m n false)
                    m3 (reduce (fn [m n] (df-visit1 g m n)) m2 (get g n))]
                (when m3
                  (-> m3
                      (assoc n true)
                      (update ::tks conj n))))))

;; m is "marked" map, perm true, temp false, unmarked if not present/nil.
;; special key ::tks has the vector of nodes in topo order.
;;; refactored df-visit as closure over graph, still recursive.

;;; slower for small examples, much faster for bigger
(defn df-topo [graph]
  (let [df-visit (fn df-visit [m n]
                   (when m
                     (case (get m n)
                       true m
                       false nil
                       (some-> (reduce df-visit (assoc m n false) (get graph n))
                               (assoc n true)
                               (update ::tks conj n)))))]
    (::tks (reduce df-visit {::tks []} (keys graph)))))


;;; doesn't seem to be worth it to used (reduced nil) to break out of reduce of bad example

;;; instead of constant ::tks, it might be slightly safer to generate a private keyword like:
#_ (def tks (keyword (gensym)))


;;; idea: make perfect hash using 63 bits of long, 0 means not applicable so you need a real hash

;;; regular (hash x) => 32 bit Integer.  I want a 6 bit index.  Maybe generate a case
;;; statement.  Maybe find a bit field from hash codes that guarantees unique among those
;;; keys.

(defn hash63x
  ([i] (bit-and 63 (hash i)))
  ([i shift] (bit-and 63 (bit-shift-right (hash i) shift))))


(defn hash63slow
  ([i]
   (let [h (bit-and 63 (hash i))]
     (first (filter pos? [h (bit-and 63 i) 31]))))

  ([i shift]
   (let [h (bit-and 63 (bit-shift-right (hash i) shift))]
     (first (filter pos? [h (bit-and 63 i) 31])))) )


(defn hash63a
  ([i]
   (let [h (bit-and 63 (hash i))]
     (if (pos? h)
       h
       (let [b (bit-and 63 i)]
         (if (pos? b) b 31)))))

  ([i shift]
   (let [h (bit-and 63 (bit-shift-right (hash i) shift))]
     (if (pos? h)
       h
       (let [b (bit-and 63 i)]
         (if (pos? b) b 31))))))


(defn hash63 [i]
  (let [h (hash i)
        b (bit-and 63 (bit-xor h (bit-shift-right h 16)))]
     (if (pos? b)
       b
       (let [b (bit-and 63 i)]
         (if (pos? b) b 31)))))

;; do shift later
