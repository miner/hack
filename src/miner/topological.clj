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
;;; the check.  I added a variant called `ss-topo` that accepts a sort-fn to standardize
;;; ordering of nodes (default is `identity` so no specific order, pass `sort` for standard
;;; Clojure ordering of "ties").  I have not figured out how to sort ties with the
;;; depth-first solution.

(defn topological? [graph topo-sorted]
  (let [kset (reduce (fn [seen x]
                       (if (and (not (seen x))
                                (every? seen (get graph x)))
                         (conj seen x)
                         (reduced #{})))
                     #{}
                     topo-sorted)]
    (every? kset (keys graph))))

;;; My original version was buggy as it passed degenerate cases that didn't cover all the
;;; keys.  Or topo-sorted seqs that had strange keys.

(defn all-nodes [g]
  (reduce into (set (keys g)) (vals g)))

(defn missing-keys [g]
  (reduce disj (all-nodes g) (keys g)))

;; Returns a new graph augmenting g with additional keys as necessary to cover all the nodes
;; mentioned in (vals g).  The added keys map to the empty set.
(defn complete-keys [g]
  (reduce (fn [g v] (reduce (fn [g k] (if (get g k) g (assoc g k #{}))) g v))
          g
          (vals g)))

;;; inverts sense of graph link direction
(defn invert-link-sense [g]
  (reduce-kv (fn [g k v] (reduce (fn [g x] (assoc g x (conj (get g x #{}) k))) g v))
             {}
             g))

;;; convert from adjacency map, essentially reversing sense of links
(defn convert-adj [adj]
  (complete-keys (invert-link-sense adj)))







;;; derived from Rosetta Code
;;; https://rosettacode.org/wiki/Topological_sort#Clojure
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

(def hhh (complete-keys {:g #{:b :c :a} :f #{:d :e}}))

;;; => [:d :e :a :b :c :f :g] with many variants
;;; canonical [:a :b :c :d :e :f :g]


;;; big example
(defonce bbb (reduce (fn [m i] (assoc m i (into #{100} (range i)))) {100 #{}} (range 100)))

;;; big and bad
(defonce xxx (assoc bbb 100 #{50}))

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


;;; The `sort-fn` can be used to standardize multiple valid orderings.  Use sort-fn = sort
;;; if you want a canonical result.  The default is `identity` which does no sorting.

(defn ss-topo
  ([graph] (ss-topo identity graph))
  ([sort-fn graph]
   (loop [tks [] g graph]
     (if (empty? g)
       tks
       (when-let [ks (reduce-kv (fn [ks k v] (if (seq v) ks (conj ks k))) nil g)]
         (recur (into tks (sort-fn ks))
                (update-vals (reduce dissoc g ks) #(reduce disj % ks))))))))



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


;; slightly faster, but not as pretty, not worth it
(defn df-topo51 [graph]
  (let [df-visit (fn df-visit [m n]
                   (if-not m
                     (reduced nil)
                     (case (get m n)
                       true m
                       false (reduced nil)
                       (if-let [mm (reduce df-visit (assoc m n false) (get graph n))]
                         (-> mm
                             (assoc n true)
                             (update ::tks conj n))
                         (reduced nil)))))]
    (::tks (reduce df-visit {::tks []} (keys graph)))))


;;; safer, but not really necessary

(defonce tks (gensym))

(defn df-topo3 [graph]
  (let [df-visit (fn df-visit [m n]
                   (when m
                     (case (get m n)
                       true m
                       false nil
                       (some-> (reduce df-visit (assoc m n false) (get graph n))
                               (assoc n true)
                               (update tks conj n)))))]
    (get (reduce df-visit {tks []} (keys graph)) tks)))


;;; doesn't seem to be worth it to used (reduced nil) to break out of reduce of bad example

;;; instead of constant ::tks, it might be slightly safer to generate a private keyword like:
#_ (def tks (keyword (gensym)))


;;; Christophe Grand on mailing list Nov 16, 2009
;;; result doesn't agree with mine???  I think this is wrong!
(defn cg-topological-sort [x]
  (mapcat
   #(for [[k v] % :when (empty? v)] k)
   (take-while seq
               (iterate #(into {} (for [[k v] % :when (seq v)] [k (mapcat % v)])) x))))



;;; also on mailing list, from John Harrop
;;; slightly hacked by SEM
;;; results agree with me, so that's good!  Not so fast.
(defn jh-order-nodes [deps]
  (let [find-a-node (fn [deps already-have-nodes]
                      (some (fn [[k v]] (if (empty? (remove already-have-nodes v)) k)) deps))]
    (loop [deps deps already-have-nodes #{} output []]
      (if (empty? deps)
        output
        (when-let [item (find-a-node deps already-have-nodes)]
          (recur
           (dissoc deps item)
           (conj already-have-nodes item)
           (conj output item)))))))


;;;; See also Stack Overflow
;;; https://stackoverflow.com/questions/22796705/ordering-of-results-based-upon-dependencies

;;; slightly hacked by SEM to agree with my result style (mapcat)
;;; does NOT detect cycles! so fails my tests
;;; slow anyway
(defn tsort [m] 
  (let [depth (fn depth [x] 
                (if (empty? (m x)) 
                  0 
                  (->> x m (map depth) (apply max) inc)))]
    (mapcat val (sort-by key (group-by depth (keys m))))))


;;; good suggestion to use Loom
;;; https://github.com/aysylu/loom

;;; looks like a good article but uses other libs so I didn't get into it
;;; https://dnaeon.github.io/graphs-and-clojure/
