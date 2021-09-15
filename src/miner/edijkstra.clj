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

;; But use reduce-kv were possible.  Also, should keep costs in a priority map.
;; [org.clojure/data.priority-map "1.0.0"]



(require '[clojure.data.priority-map :refer [priority-map]])

;; convert to graph form from path-map
(defn make-graph [path-map]
  (reduce-kv assoc-in
             (zipmap (map first (keys path-map)) (repeat (priority-map)))
             path-map))

(defn neighbors [g k unvisited]
  (select-keys (get g k) unvisited))

(defn update-costs [g costs curr unvisited]
  (let [curr-cost (costs curr)]
    (-> (reduce-kv (fn [c nbr nbr-cost] (update-in c [nbr] (partial min (+ curr-cost nbr-cost))))
                   costs
                   (neighbors g curr unvisited))
        (dissoc curr))))


(defn dijkstra [path-map start end]
  (let [g (make-graph path-map)]
    (loop [unvisited (disj (into #{} cat (keys path-map)) start)
           path [start]
           cost-map (reduce #(assoc % %2 Long/MAX_VALUE) (priority-map start 0) unvisited)]
      (cond (= (peek path) end) path
            (empty? unvisited) nil
            :else (let [cost-map (update-costs g cost-map (peek path) unvisited)
                        best (peek cost-map)
                        node (key best)]
                    (when (< (val best) Long/MAX_VALUE)
                      (recur (disj unvisited node)
                             (conj path node)
                             cost-map)))))))





(def sample {[:a :b] 1
             [:a :c] 2
             [:c :a] 4
             [:c :d] 5
             [:d :a] 6
             [:d :c] 1})

(defn smoke-path [shortest-path]
  (let [graph {[:a :b] 1
               [:a :c] 2
               [:c :a] 4
               [:c :d] 5
               [:d :a] 6
               [:d :b] 7
               [:d :c] 1}]
    (assert (= (shortest-path graph :d :a) [:d :c :a]))
    (assert (= (shortest-path graph :c :b) [:c :a :b]))
    (assert (= (shortest-path graph :a :a) [:a]))
    (assert (= (shortest-path graph :a :b) [:a :b]))
    (assert (= (shortest-path graph :d :b) [:d :c :a :b]))
    (assert (nil? (shortest-path graph :b :d) ))
    (assert (nil? (shortest-path graph :b :c) )))
  true)
