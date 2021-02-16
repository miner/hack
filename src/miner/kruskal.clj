(ns miner.kruskal
  (:require [clojure.set :as set]))

;; Blog post by Atabey Kaygun
;; https://kaygun.tumblr.com/post/643010859143151616/kruskals-algorithm-implemented-in-clojure


;; https://en.wikipedia.org/wiki/Kruskal%27s_algorithm
;; Kruskal’s algorithm finds a spanning tree within a graph


(def graph #{#{0 1} #{1 2} #{2 3} #{0 3} #{3 4} #{2 4} #{0 5}})

(defn creates-a-cycle? [path tree]
  (let [e (->> tree (filter #(not (empty? (set/intersection path %)))) first)]
    (cond (nil? e) false
          (set/subset? e path) true
          :else (recur (set/union path e) (remove #{e} tree)))))


#_
(creates-a-cycle? #{0 1} #{#{1 2} #{2 3} #{3 0}})
;;=> true

#_
(creates-a-cycle? #{0 1} #{#{1 2} #{2 3}})
;;=> false


(defn kruskal [graph]
   (loop [H (into [] graph)
          tree #{}]
      (if (empty? H)
          tree
          (let [edge (rand-nth H)]
             (recur (remove #{edge} H)
                    (if (creates-a-cycle? edge tree)
                        tree
                        (conj tree edge)))))))

#_
(kruskal graph)
;;=> #{#{4 3} #{0 1} #{4 2} #{1 2} #{0 5}}
;; SEM actually gets several different answers.  Naturally rand-nth changes the results.


(def railway-connections 
 #{
   #{"Birmingham" "Sheffield"}
   #{"Birmingham" "Leeds"}
   #{"Birmingham" "Bristol"}
   #{"Birmingham" "Liverpool"}
   #{"Birmingham" "Manchester"}

   #{"Bristol" "Leeds"}
   #{"Bristol" "Liverpool"}
   #{"Bristol" "Manchester"}

   #{"Leeds" "Liverpool"}
   #{"Leeds" "Manchester"}

   #{"Liverpool" "Manchester"}

   #{"London" "Birmingham"}
   #{"London" "Sheffield"}
   #{"London" "Leeds"}
   #{"London" "Bristol"}
   #{"London" "Liverpool"}
   #{"London" "Manchester"}

   #{"Sheffield" "Leeds"}
   #{"Sheffield" "Liverpool"}
   #{"Sheffield" "Manchester"}
})

#_
(kruskal railway-connections)

;;=>
#_ #{#{"Sheffield" "Leeds"} #{"Manchester" "Liverpool"} #{"Birmingham" "Liverpool"}
     #{"Manchester" "London"} #{"Liverpool" "Bristol"} #{"Leeds" "London"}}



;;;;;;;;;;; SEM hacking

;; BUG in old sem-cycle? (deleted) -- can't just reduce (one pass) because it needs to
;; recycle missing nodes in case something connects later


;; SEM -- need to simplify tree and edge access (original overuses set fns)
;; tree is available edges, e is one that is at least partially connected to path
;; check if both ends of e is in (shows cycle).  Must connect at one end first.
(defn sem-cycle? [path tree]
  (when-let [e (first (filter #(some path %) tree))]
    (or (set/subset? e path)
        (recur (set/union path e) (disj tree e)))))

;;; use reduce, much simpler than original looping
(defn semk [graph]
  (reduce (fn [tree edge]
            (if (sem-cycle? edge tree)
              tree
              (conj tree edge)))
          #{}
          graph))


(defn smoke-kruskal
  ([] (smoke-kruskal kruskal))
  ([kruskal]
   (assert (= (count (kruskal graph)) 5))
   (assert (= (count (kruskal railway-connections)) 6))
   true))


;; renamed and reordered args
(defn will-cycle? [tree nodes]
  (when-let [edge (first (filter #(some nodes %) tree))]
    (or (set/subset? edge nodes)
        (recur (disj tree edge) (set/union nodes edge)))))

(defn kru [graph]
  (reduce (fn [tree edge]
            (if (will-cycle? tree edge)
              tree
              (conj tree edge)))
          #{}
          graph))


;; when-first FTW!

(defn kru1 [graph]
  (let [will-cycle? (fn [tree nodes]
                      (when-first [edge (filter #(some nodes %) tree)]
                        (or (set/subset? edge nodes)
                            (recur (disj tree edge) (set/union nodes edge)))))]
    (reduce (fn [tree edge]
              (if (will-cycle? tree edge)
                tree
                (conj tree edge)))
            #{}
            graph)))



;; BUGGY with all at once -- you need to make connections

