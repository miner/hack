(ns miner.topological)

;;; https://blog.exupero.org/topological-sort-in-clojure/

;;; https://en.wikipedia.org/wiki/Topological_sorting
;;;
;;; a topological sort or topological ordering of a directed graph is a linear ordering of
;;; its vertices such that for every directed edge uv from vertex u to vertex v, u comes
;;; before v in the ordering

;;; Most of these solutions are based on Kahn's algorithm.  There is another way of doing it
;;; using depth first search.  (Not implemented here.)  My version is sem-topo which is
;;; significantly faster due to slightly rearranged code.


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


(defn test-topo [topological-sort]
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
