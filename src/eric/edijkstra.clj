(ns miner.edijkstra)

;; https://gist.github.com/ericnormand/732246722ef3d632bb5c6aa391a8a899


;; Dijkstra's Algorithm
;; 
;; Write a function to calculate the shortest path between two nodes in a graph using
;; Dijkstra's Algorithm. The graph is directional and is represented as a map. The keys are
;; pairs of nodes and the value is the cost to traverse the edge between them, from the first
;; node to the second node. If a pair of nodes does not exist in the map, it means that there
;; is no edge between them.  Your function should return a path, consisting of a sequence of
;; the names of the nodes to follow, including the starting and ending nodes.

;; https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm


;; my code is inspired by:
;; http://loganlinn.com/blog/2013/04/22/dijkstras-algorithm-in-clojure/

;; But use reduce-kv were possible.  Also, could keep costs in a priority map.
;; [org.clojure/data.priority-map "1.1.0"] So far, priority-map is slower.  Probably due to
;; need for filtering by unvisited.  How could the representation combine unvisited?  Tried,
;; but not faster


;;; 01/06/22  02:10 by miner -- new idea:  apply this to cgrand's A* implementation to keep
;;; full path rather than calculating at end from parents.  Also consider cheaper lazy
;;; priority map with min search backup


;;; seems like a map from terminal node to cost would work but the twist is that the cost is
;;; a vector of nodes and peek int cost.  Decided to elide the INF (just don't include key,
;;; assume MAX if you need it).

;; Actual example of good cost-map
#_
{:b [:c :b 5]
 :c [:c 0]
 ::unvisited #{:a :b :d}}



(defn best-unvisited-path [cost-map]
  (let [unvisited (::unvisited cost-map)]
    (pop (reduce-kv (fn [best k v]
                      (if (and (contains? unvisited k) (< (peek v) (peek best)))
                        v
                        best))
                    [Long/MAX_VALUE]
                    cost-map))))


;; assuming no explicit self-links [:x :x], it's safe to remove the node from unvisited before
;; looking for neighbors

(defn update-costs [g node cost-map]
  (let [node-cost (peek (cost-map node))
        node-path (pop (cost-map node))
        unvisited (disj (::unvisited cost-map) node)]
    (reduce-kv (fn [cm nbr nbr-step]
                 (if-not (contains? unvisited nbr)
                   cm
                   (let [new-cost (+ node-cost nbr-step)
                         nbr-path-cost (get cm nbr [Long/MAX_VALUE])]
                     (if (< new-cost (peek nbr-path-cost))
                       (assoc cm nbr (conj node-path nbr new-cost))
                       cm))))
               (assoc cost-map ::unvisited unvisited)
               (get g node))))


(defn shortest-path [path-map start end]
  (let [g (reduce-kv assoc-in {} path-map)]
    (loop [cost-map {start [start 0] ::unvisited (into #{} cat (keys path-map))}]
      (let [best-path (best-unvisited-path cost-map)]
        (when-let [node (peek best-path)]
          (if (= node end)
            best-path
            (recur (update-costs g node cost-map))))))))

;; In update-costs, if node has been visited and doesn't result in solution, can we delete
;; it?  You will never try it again, I think, since it's no long unvisited.  Seems like
;; update-costs should delete the cost entry as well as the unvisited.  Seems like it works,
;; but it didn't improve performance.  Problem could be another path to that node would
;; waste time recalculating minimum that maybe you already knew.  But actually, we wouldn't
;; visit it again as only unvisited get to expand.  So it should be OK after all.  But it's
;; not faster in my tests.  Maybe with a larger graph it would be worth pruning.


(def sample {[:a :b] 1
             [:a :c] 2
             [:c :a] 4
             [:c :d] 5
             [:d :a] 6
             [:d :b] 7
             [:d :c] 1})


;; use this to make another big example
(defn gen-big-rand []
  (let [nodes (into [] (map #(keyword (str (char %)))) (range (int \a) (int \z)))]
    (-> {}
        (into (for [[a b] (partition 2 (shuffle nodes))]
                [[a b] (inc (rand-int 5))]))
        (into (for [[a b] (partition 2 (shuffle nodes))]
                [[a b] (inc (rand-int 5))]))
        (into (for [[a b] (partition 2 (shuffle nodes))]
                [[a b] (inc (rand-int 5))]))
        (into (for [[a b] (partition 2 (shuffle nodes))]
                [[a b] (inc (rand-int 5))]))
        (into (for [[a b] (partition 2 (shuffle nodes))]
                [[a b] (inc (rand-int 5))]))
        (into (for [[a b] (partition 2 (shuffle nodes))]
                [[a b] (inc (rand-int 5))]))
        (into (for [[a b] (partition 2 (shuffle nodes))]
                [[a b] (inc (rand-int 10))])))))

;;; Don't change this without fixing smoke-path
(def big
  {[:p :x] 8, [:l :d] 3, [:c :q] 1, [:j :n] 3, [:s :o] 2, [:g :t] 4, [:b :t] 8, [:x :d] 4,
   [:g :v]   4, [:u :m] 4, [:o :i] 4, [:l :k] 1, [:k :a] 3, [:b :l] 4, [:w :h] 2,
   [:x :j] 2, [:u :q] 3,   [:h :k] 4, [:i :o] 5, [:h :e] 9, [:f :e] 2, [:r :s] 2, [:i :v] 2,
   [:b :p] 5, [:d :n] 2, [:j :h]   2, [:u :y] 4, [:v :g] 1, [:n :i] 5, [:f :l] 1, [:w :p] 4,
   [:d :j] 8, [:p :y] 4, [:l :o] 2,   [:n :p] 4, [:e :x] 3, [:n :v] 1, [:x :t] 5, [:t :o] 4,
   [:s :n] 1, [:p :v] 2, [:c :o] 1, [:u :k]   4, [:b :f] 2, [:a :n] 5, [:f :d] 4, [:u :i] 5,
   [:e :b] 1, [:u :d] 2, [:k :r] 2, [:x :e] 4,   [:w :q] 3, [:y :g] 2, [:j :g] 1, [:f :v] 5,
   [:t :w] 4, [:o :f] 3, [:p :c] 5, [:d :s] 1, [:e :k]   3, [:m :q] 1, [:i :c] 2, [:j :m] 4,
   [:t :s] 1, [:r :x] 5, [:w :c] 1, [:y :r] 7, [:h :a] 1, [:y :a] 2, [:g :l] 5, [:i :r] 2,
   [:h :q] 3, [:e :m] 1, [:y :c] 3, [:m :a] 2, [:s :w] 3, [:m :h]   5, [:q :c] 4})



(def wikipedia-example {[:1 :2] 7
                        [:1 :6] 14
                        [:1 :3] 9

                        [:2 :1] 7
                        [:2 :3] 10
                        [:2 :4] 15

                        [:3 :1] 9
                        [:3 :2] 10
                        [:3 :6] 2
                        [:3 :4] 11

                        [:4 :2] 15
                        [:4 :3] 11
                        [:4 :5] 6

                        [:5 :4] 6
                        [:5 :6] 9

                        [:6 :1] 14
                        [:6 :3] 2
                        [:6 :5] 9})



(defn smoke-path [shortest-path]
  (let [graph {[:a :b] 1
               [:a :c] 2
               [:c :a] 4}]
    (assert (= (shortest-path graph :c :b) [:c :a :b]))
    (assert (= (shortest-path graph :a :a) [:a]))
    (assert (= (shortest-path graph :a :b) [:a :b]))
    (assert (nil? (shortest-path graph :b :c) )))
  (let [graph sample]
    (assert (= (shortest-path graph :d :a) [:d :c :a]))
    (assert (= (shortest-path graph :c :b) [:c :a :b]))
    (assert (= (shortest-path graph :a :a) [:a]))
    (assert (= (shortest-path graph :a :b) [:a :b]))
    (assert (= (shortest-path graph :d :b) [:d :c :a :b]))
    (assert (= (shortest-path graph :d :a) [:d :c :a]))
    (assert (= (shortest-path graph :d :c) [:d :c]))
    (assert (nil? (shortest-path graph :b :d) ))
    (assert (nil? (shortest-path graph :b :c) )))
  (let [graph big]
    (assert (= (shortest-path graph :a :b) [:a :n :i :c :o :f :e :b]))
    (assert (= (shortest-path graph :b :j) [:b :f :e :x :j]))
    (assert (= (shortest-path graph :c :y) [:c :o :f :e :b :p :y]))
    (assert (= (shortest-path graph :d :k) [:d :s :o :f :l :k]))
    (assert (nil? (shortest-path graph :d :u))))
  ;; test from @KingCode
  (let [graph wikipedia-example]
    (assert (= (shortest-path graph :1 :5)  [:1 :3 :6 :5]))
    (assert (= (shortest-path graph :5 :4) [:5 :4]))
    (assert (= (shortest-path graph :5 :3) [:5 :6 :3]))
    (assert (= (shortest-path graph :3 :5) [:3 :6 :5]))
    (assert (= (shortest-path graph :2 :6) [:2 :3 :6])))
  true)




;;; My guess is that priority-map fiddling isn't worth it because the filtering for
;;; unvisited defeats the main purpose.  Also, there's a lot of bookkeeping that has to be
;;; done on every access.  Searching later is a reasonable trade-off.

;;; Consider how to use priority-map rather than searching for best current path.
(require '[clojure.data.priority-map :refer [priority-map priority-map-keyfn]])

;; (priority-map-keyfn peek)



;;; BUT NOT FASTER
(defn best-unvisited-path8 [cost-map unvisited]
  (reduce (fn [_ kv]
            (if (contains? unvisited (key kv))
              (reduced (pop (val kv)))))
          [Long/MAX_VALUE]
          cost-map))

;; assuming no explicit self-links [:x :x], it's safe to remove the node from unvisited before
;; looking for neighbors

;; FIXME  (dissoc cm nbr)  maybe too much
(defn update-costs8 [g node cost-map unvisited]
  (let [node-cost (peek (cost-map node))
        node-path (pop (cost-map node))]
    (reduce-kv (fn [cm nbr nbr-step]
                 (if-not (contains? unvisited nbr)
                   cm
                   (let [new-cost (+ node-cost nbr-step)
                         nbr-path-cost (get cm nbr [Long/MAX_VALUE])]
                     (if (< new-cost (peek nbr-path-cost))
                       (assoc cm nbr (conj node-path nbr new-cost))
                       cm))))
               cost-map
               (get g node))))


(defn pmdijk8 [path-map start end]
  (let [g (reduce-kv assoc-in {} path-map)]
    (loop [cost-map (priority-map-keyfn peek start [start 0])
           unvisited (into #{} cat (keys path-map))]
      (let [best-path (best-unvisited-path8 cost-map unvisited)
            node (peek best-path)]
        (cond (nil? node) nil
              (= node end) best-path
              :else (recur (update-costs8 g node cost-map unvisited) (disj unvisited node)))))))






;; new idea: nested maps so g and cost-map and unvisited is all in one map

;; could submap :path and :cost instead of decoding vector -- implemented below, but not winner
;; could :visited into map, but that's more linear searching for unvisited, tried below but
;; not a winner.
;;
;; priority-by-keyfn :cost

#_
{:b {:cost 5 :path [:c :b] :visited false :neighbors nil}
 :c {:cost 0 :path [:c] :visited true :neighbors {:a 4, :d 5}}
 :a {:cost MAX :path nil :visited false :neighbors nil}
 :d {:cost MAX :path nil :visited false :neighbors nil}}


;;; priority-map-keyfn was buggy with reduce-kv.  Filed Bug DPRIMAP-14 and it was fixed for
;;; version 1.1.0.

(defn best-unvisited-path9 [cost-map]
  (first (keep (fn [me] (let [v (val me)]
                          (when-not (:visited v)
                            (:path v))))
               cost-map)))


(defn update-costs9 [cost-map node]
  (let [node-cost (:cost (cost-map node))
        node-path (:path (cost-map node))]
    (reduce-kv (fn [cm nbr nbr-step]
                 (if (:visited (cost-map nbr))
                   cm
                   (let [new-cost (+ node-cost nbr-step)
                         nbr-state (get cm nbr)]
                     (if (< new-cost (:cost nbr-state))
                       (assoc cm nbr (assoc nbr-state :path (conj node-path nbr) :cost new-cost))
                       cm))))
               (assoc-in cost-map [node :visited] true)
               (:neighbors (cost-map node)))))

;; maybe clever to combine state, but slow (4x shortest).  Also, priority-map is slow -- at
;; least for small graphs.
(defn pmdijk9 [path-map start end]
  (let [nodes (into #{} cat (keys path-map))
        neighbors (reduce-kv assoc-in {} path-map)
        init-cost-map (-> (priority-map-keyfn :cost start {:cost 0 :path [start]})
                        (into (zipmap (disj nodes start)
                                      (repeat {:cost Integer/MAX_VALUE}))))
        init-cost-map (reduce-kv (fn [cm k neighbor-map]
                                   (assoc-in cm [k :neighbors] neighbor-map))
                                 init-cost-map
                                 neighbors)]
    (loop [cost-map init-cost-map]
      (let [best-path (best-unvisited-path9 cost-map)
            node (peek best-path)]
        ;;(println "Cm9" cost-map best-path)
        (cond (nil? node) nil
              (= node end) best-path
              :else (recur (update-costs9 cost-map node)))))))




;;; ----------------------------------------------------------------------
;;; like dijk9 but with conventional map.  Much faster.  Of course, we're using a small test
;;; graph so the overhead of the priority-map isn't worth it.  Even with my "big" test, it's
;;; still slower with priority-map.  My conventional style dijk5 is about 20% slower than my
;;; preferred shortest-path.

(defn best-unvisited-path5 [cost-map]
  (:path  (reduce-kv (fn [best k v]
                       (if (and (not (:visited v)) (< (:cost v) (:cost best)))
                         v
                         best))
                     {:cost Integer/MAX_VALUE}
                     cost-map)))


(defn update-costs5 [cost-map node]
  (let [node-cost (:cost (cost-map node))
        node-path (:path (cost-map node))]
    (reduce-kv (fn [cm nbr nbr-step]
                 (if (:visited (cost-map nbr))
                   cm
                   (let [new-cost (+ node-cost nbr-step)
                         nbr-state (get cm nbr)]
                     (if (< new-cost (:cost nbr-state Integer/MAX_VALUE))
                       (assoc cm nbr (assoc nbr-state :path (conj node-path nbr) :cost new-cost))
                       cm))))
               (assoc-in cost-map [node :visited] true)
               (:neighbors (cost-map node)))))

(defn dijk5 [path-map start end]
  (let [init-cost-map (reduce-kv (fn [cm kk cost]
                                   (assoc-in cm [(first kk) :neighbors (second kk)] cost))
                                 (reduce (fn [cm k] (assoc cm k  {:cost Integer/MAX_VALUE}))
                                         {start {:cost 0 :path [start]}}
                                         (disj (into #{} (comp (map key) (map first))
                                                     path-map) start))
                                path-map)]
    (loop [cost-map init-cost-map]
      (let [best-path (best-unvisited-path5 cost-map)
            node (peek best-path)]
        ;;(println "Cm5" cost-map best-path)
        (cond (nil? node) nil
              (= node end) best-path
              :else (recur (update-costs5 cost-map node)))))))


;;; note: priority-maps cannot use transient
