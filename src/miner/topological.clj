(ns miner.topological)

;;; https://blog.exupero.org/topological-sort-in-clojure/

;;; https://en.wikipedia.org/wiki/Topological_sorting
;;;
;;; a topological sort or topological ordering of a directed graph is a linear ordering of
;;; its vertices such that for every directed edge uv from vertex u to vertex v, u comes
;;; before v in the ordering


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

(def ggg {:a #{:b :c :d}
              :b #{:c :d}
              :c #{:d :e}
              :d #{}
          :e #{:d}})

(def circ {:a #{:b}
           :b #{:c}
           :c #{:a}})


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


;;; Fastest
(defn sem-topo [graph]
  (loop [tks [] g graph]
    (if (empty? g)
      tks
      (when-let [ks (reduce-kv (fn [ks k v] (if (seq v) ks (conj ks k))) nil g)]
        (recur (into tks ks)
               (update-vals (reduce dissoc g ks) #(reduce disj % ks)))))))



;;; Slower to build up intermediate ks as a set
(defn sem-topo4 [graph]
  (loop [tks [] g graph]
    (if (empty? g)
      tks
      (let [ks (reduce-kv (fn [ks k v] (if (seq v) ks (conj ks k))) #{} g)]
        (when (seq ks)
          (recur (into tks ks)
                 (update-vals (reduce dissoc g ks) #(clojure.set/difference % ks))))))))

