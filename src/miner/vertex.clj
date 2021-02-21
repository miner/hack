(ns miner.vertex)

;; vertex cover problem for a given undirected graph
;;
;; https://kaygun.tumblr.com/post/643590629046288384/calculating-vertex-covers-in-clojure
;;
;; https://en.wikipedia.org/wiki/Vertex_cover

;; Kaygun's original code

(def G {0 #{1 2 3}, 1 #{0 2}, 2 #{0 1 3 4}, 3 #{0 2}, 4 #{2}})

(defn largest-degree [G]
  (->> (into [] G)
       (map (fn [[x y]] [x (count y)]))
       (sort #(> (last %1) (last %2)))
       first
       first))

(defn remove-vertex [v G]
   (->> (dissoc G v)
        (map (fn [x] {(key x) (->> (val x)
                                   (remove #{v})
                                   (into #{}))}))
        (into {})))


(defn cover 
  ([G] (cover G []))
  ([G S] (let* [v (largest-degree G)
                H (remove-vertex v G)]
            (if (or (empty? G)
                    (empty? (G v)))
                S
                (recur H (conj S v))))))

;; "How do we know that we get the correct answer? Well, if we remove all of the vertices in
;; the cover, what remains has to be an independent subset."

(defn verify-independent-subset [coverfn rmvfn G]
  (loop [S (coverfn G)
         H G]
    (if (empty? S)
      (and (every? G (keys H))
           (every? empty? (vals H)))
      (recur (rest S) (rmvfn (first S) H)))))

(defn random-tree [n]
   (->> (range 1 (* 2 n))
        (map (fn [x] {(rand-int x) #{x}}))
        (reduce  #(merge-with clojure.set/union %1 %2))
        (into {})))

(defonce G100 (random-tree 100))



;; SEM code

(defn smoke-cover
  ([] (smoke-cover cover remove-vertex))
  ([cover remove-vertex]
   (assert (= (cover G) [2 0]))
   (assert (verify-independent-subset cover remove-vertex G))
   (assert (verify-independent-subset cover remove-vertex G100))
   true))


(defn max-degree [g]
  (when-not (empty? g)
    (key (apply max-key #(count (val %)) g))))

(defn rm-vertex [v G]
  (let [g (dissoc G v)]
    (reduce (fn [m vtx] (update m vtx disj v))
            g
            (keys g))))

(defn cov [G]
  (loop [G G S []]
    (let [v (max-degree G)]
      (if (or (empty? G) (empty? (G v)))
        S
        (recur (rm-vertex v G) (conj S v))))))



;; SEM it would be convenient to use arity to combine `cover` and `remove-vertex`.
;; I think the G graph should be first arg and v vertex second.

(defn semcov
  ([G]
   (let [max-degree (fn [g]
                      (when-not (empty? g)
                        (key (apply max-key #(count (val %)) g))))]
     (loop [G G S []]
       (let [v (max-degree G)]
         (if (or (empty? G) (empty? (G v)))
           S
           (recur (semcov G v) (conj S v)))))))
  ([G v]
   (let [g (dissoc G v)]
     (reduce (fn [m vtx] (update m vtx disj v))
             g
             (keys g)))))



(defn semcov2
  ([G]
   (let [max-degree (fn [g]
                      (when-not (empty? g)
                        (key (apply max-key #(count (val %)) g))))]
     (loop [G G S []]
       (let [v (max-degree G)]
         (if (or (empty? G) (empty? (G v)))
           S
           (recur (semcov2 G v) (conj S v)))))))
  ([G v]
   (let [g (dissoc G v)]
     (persistent! (reduce-kv (fn [m vtx vs] (assoc! m vtx (disj vs v)))
             (transient {})
             g)))))

(defn verify-independent-semcov [semcov G]
  (loop [S (semcov G)
         H G]
    (if (empty? S)
      (and (every? G (keys H))
           (every? empty? (vals H)))
      (recur (rest S) (semcov H (first S))))))



(defn smoke-semcov
  ([] (smoke-semcov semcov))
  ([semcov]
   (assert (= (semcov G) [2 0]))
   (assert (verify-independent-semcov semcov G))
   (assert (verify-independent-semcov semcov G100))
   true))
