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

;;; So far, priority-map is slower.  Probably due to need for filtering by unvisited.  How
;;; could the representation combine unvisited?

;;; Uncertain about correctness of my code.  May be missing backtracking effect of costs.  Want
;;; to keep full path as keys or something.  The problem is the costs need to be expanded
;;; correctly.  You can't assume the lowest cost is connected.  It might be picking up an
;;; old trail.  Start over.


;;; seems like a map from terminal node to cost would work but the twist is that the cost is
;;; a vector of nodes and peek int cost.

#_   ;; for example
{:a [INF]
 :b [:c :b 5]
 :c [:c 0]
 :d [INF]
 :e [INF]
 :f [INF]}
;; maybe part of state, currently separate
#_ {::unvisited #{:d :e :f}}


#_
{:b [:c :b 5]
 :c [:c 0]
 ::unvisited #{:a :b :d}}

;; could submap :path and :cost instead of decoding vector
#_
{:b {:path [:c :b] :cost 5}
 :c {:path [:c] :cost 0}
 ::unvisited #{:a :b :d}}

;; could :visited into map, but that's more linear searching for unvisited

#_
{:b {:path [:c :b] :cost 5 :visited false}
 :c {:path [:c] :cost 0 :visited true}
 :a {:visited false}
 :d {:visited true}}


;; Decided to leave INF nodes out of costs so if node key is missing it means [INF].
(defn best-unvisited-path6 [cost-map unvisited]
  (let [best (apply min-key
                    (fn [node]
                      (if-let [path-cost (cost-map node)]
                        (peek path-cost)
                        Long/MAX_VALUE))
                    unvisited)]
    (pop (cost-map best))))

(defn update-costs6 [g costs node unvisited]
  (let [node-cost (peek (costs node))
        node-path (pop (costs node))]
    (reduce-kv (fn [c nbr nbrcost]
                 (let [new-cost (+ node-cost nbrcost)
                       nbrcost (get c nbr [Long/MAX_VALUE])]
                   (if (< new-cost (peek nbrcost))
                     (assoc c nbr (conj node-path nbr new-cost))
                     c)))
               costs
               (select-keys (get g node) unvisited))))

(defn dijk6 [path-map start end]
  (let [g (reduce-kv assoc-in {} path-map)]
    (loop [unvisited (into #{} cat (keys path-map))
           cost-map {start [start 0]}]
      ;;(println "Cmap6 top loop" cost-map unvisited)
      (let [best-path (best-unvisited-path6 cost-map unvisited)
            node (peek best-path)]
        ;;(println "Best6" best-path)
        (cond (nil? node) nil
              (= node end) best-path
              (empty? unvisited) nil
              :else (let [cost-map (update-costs6 g cost-map node unvisited)
                          unvisited (disj unvisited node)]
                      (recur unvisited cost-map)))))))



;;; idea: use single state cost-map with ::unvisited key to hold unvisited

(defn best-unvisited-path7 [cost-map]
  (let [best (apply min-key
                    (fn [node]
                      (if-let [path-cost (cost-map node)]
                        (peek path-cost)
                        Long/MAX_VALUE))
                    (cost-map ::unvisited))]
    (pop (cost-map best))))

(defn update-costs7 [g node cost-map]
  (let [node-cost (peek (cost-map node))
        node-path (pop (cost-map node))]
    (-> (reduce-kv (fn [c nbr nbr-step]
                     (let [new-cost (+ node-cost nbr-step)
                           nbrcost (get c nbr [Long/MAX_VALUE])]
                       (if (< new-cost (peek nbrcost))
                         (assoc c nbr (conj node-path nbr new-cost))
                         c)))
                   cost-map
                   ;; unvisited neigbors
                   (select-keys (get g node) (cost-map ::unvisited)))
        ;;(dissoc node)
        (update ::unvisited disj node))))

;; if node has been visited and doesn't result in solution, can we delete it?  You will
;; never try it again, I think, since it's no long unvisited.  Seems like update-costs
;; should delete the cost entry as well as the unvisited.  Seems like it works, but it
;; didn't improve performance.   Problem could be another path to that node would waste time
;; recalculating minimum that maybe you already knew.  But actually, we wouldn't visit it
;; again as only unvisited get to expand.  So it shoul be OK after all.

;; slightly slower but nice to have single state
(defn dijk7 [path-map start end]
  (let [g (reduce-kv assoc-in {} path-map)]
    (loop [cost-map {start [start 0] ::unvisited (into #{} cat (keys path-map))}]
      ;;(println "Cmap7 top loop" cost-map)
      (let [best-path (best-unvisited-path7 cost-map)
            node (peek best-path)]
        ;;(println "Best7" best-path)
        (cond (nil? node) nil
              (= node end) best-path
              :else (recur (update-costs7 g node cost-map )))))))






(def sample {[:a :b] 1
             [:a :c] 2
             [:c :a] 4
             [:c :d] 5
             [:d :a] 6
             [:d :b] 7
             [:d :c] 1})

(defn smoke-path [shortest-path]
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
  true)




;;; Consider how to use priority-map rather than searching for best current path.
(require '[clojure.data.priority-map :refer [priority-map]])



;;; WARNING -- Buggy code below.  Wasn't correctly keeping track of non-current path
;;; possibilities.

;; FIXME: costs could be equal.  Need a better score to be consistent
;;; FIXME backtrack?
;;; FIXME why dissoc

;;; note: priority-maps cannot use transient


;;; BUG in priority-map -- reduce-kv (sometimes?) doesn't follow priority order (unlike seq)
;;; -- could be an issue with known bug about fast-path???  Work-around is to call seq first.


;; FIXME unvisited disj (key best) vs (peek path)




;;; what if you didn't have priority-map?  You would have to find the best
;;; actually much faster.  The unvisited filtering is apparently expensive with the priority map.






;;; ----------------------------------------------------------------------
;;; Junk

;;; Really don't need priority-map here!
;; convert to graph form from path-map
(defn make-graph [path-map]
  (reduce-kv assoc-in
             (zipmap (map first (keys path-map)) (repeat (priority-map)))
             path-map))


(defn best-unvisited-dijk-SLOWER [cost-map unvisited]
  (let [best (reduce (fn [bkv kv]
                       (if (and (< (nth kv 1) (nth bkv 1)) (contains? unvisited (nth kv 0)))
                         kv
                         bkv))
                     [nil Long/MAX_VALUE]
                     cost-map)]
    (when (< (nth best 1) Long/MAX_VALUE)
      (nth best 0))))

;; about same even though I think it should be faster
(defn neighbors1 [g node unvisited]
  (-> (reduce-kv (fn [m k v] (if (contains? unvisited k)
                               (assoc! m k v)
                               m))
                 (transient {})
                 (get g node))
      persistent!))

