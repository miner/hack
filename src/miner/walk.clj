(ns miner.walk
  :require [clojure.walk :as w])





;; SEM: I don't like this style with an atom for side-effects.  Should do the walk with in
;; and out params.
;; Could use a zipper instead.


;; https://github.com/metosin/schema-viz/blob/master/src/schema_viz/core.clj
#_ (defn- collect-schemas [schemas]
  (let [name->schema (atom {})]
    (stw/prewalk
      (fn [schema]
        (when-let [name (s/schema-name schema)]
          (swap!
            name->schema update-in [name]
            (fn [x] (conj (or x #{}) schema))))
        schema)
      schemas)
    ;; TODO: handle duplicate names here
    (->> @name->schema vals (map first))))


;; SEM by the way, even if you want to do something like this with an atom, you might use a
;; local var instead.  with-local-vars
;; BUT... The atom is thread-safe.  Vars are thread-local which should be fine for this usage.

;; However, the atom version seems about the same speed so there's no good reason to prefer
;; with-local-vars over the atom.  But still, it's better to be functional without having
;; the state.

(def sample {:a {:bar 1 :foo 2} :b {:bar [3 {:foo 4} 5]}})

(defn atom-foo [coll]
  (let [state (atom [])]
    (w/prewalk
      (fn [x]
        (when-let [foo (and (map? coll) (:foo x))]
          (swap! state conj foo))
        x)
      coll)
    @state))


(defn local-foo [coll]
  (with-local-vars [state []]
    (w/prewalk
      (fn [x]
        (when-let [foo (and (map? coll) (:foo x))]
          (var-set state (conj @state foo)))
        x)
      coll)
    @state))

;; simple functional style, walking and collecting in res arg
(defn foos
  ([root]
   (foos [] root))
  ([res x]
   (cond (map? x) (if-let [foo (:foo x)]
                    (recur (conj res (:foo x)) (vals x))
                    (recur res (vals x)))
         (coll? x) (into res (mapcat foos x))
       :else res)))

;; transients are actually slower for small samples
(defn tfoos
  ([root]
   (persistent! (tfoos (transient []) root)))
  ([tres x]
   (cond (map? x) (if-let [foo (:foo x)]
                    (recur (conj! tres (:foo x)) (vals x))
                    (recur tres (vals x)))
         (coll? x) (reduce conj! tres (mapcat foos x))
       :else tres)))


;; generalized to take a key to collect the values from, but structure of nested maps and
;; vectors (collections) hard-wired.
(defn collect-by-key
  ([k root]
   (collect-by-key k [] root))
  ([k res x]
   (cond (map? x) (if-let [foo (k x)]
                    (recur k (conj res foo) (vals x))
                    (recur k res (vals x)))
         (coll? x) (into res (mapcat #(collect k %) x))
       :else res)))

;; but the map? and coll? are hard-coded so it's like a baby walk
;; issue: is the f test applied to item or structure (map/coll itself)
;; maybe better to have key and f
;; but is this general purpose enough to generalize?  Maybe better to leave it as a one-of
;; hack tuned to particular application.

;; Mid hacking...
(defn collect
  ([f root]
   (collect f [] root))
  ([f res x]
   (cond (map? x) (if-let [foo (f x)]
                    (recur f (conj res foo) (vals x))
                    (recur f res (vals x)))
       (coll? x) (into res (mapcat #(collect f %) x))
       :else (if-let [foo (f x)] (conj res foo) res))))



;;; 04/02/16  10:56 by miner -- 

;; https://github.com/alandipert/intension
(defn paths
  "Returns the set of paths into a nested map/vector."
  ([root]
   {:pre [(or (map? root)
              (vector? root))]}
   (paths [] root))
  ([parent x]
   (cond (map? x)      (mapcat (fn [[k v]] (paths (conj parent k) v)) x)
         (vector? x)   (mapcat #(paths (conj parent %1) %2) (range) x)
         :else [parent])))



;; slower
(defn mpaths
  "Returns the set of paths into a nested map/vector."
  ([root]   (mpaths [] root))
  ([parent x]
   (cond (map? x)      (into [] (mapcat (fn [[k v]] (mpaths (conj parent k) v))) x)
         (vector? x)   (into [] (comp (map-indexed #(mpaths (conj parent %1) %2)) cat) x)
         :else [parent])))

(defn mkeys [x]
  (cond (map? x) (keys x)
        (vector? x) (range (count x))
      :else ()))

(defn mvals [x]
  (if (map? x) (vals x) x))

;; SEM not sure about this one.
(defn mpsXX
  "Returns the set of paths into a nested map/vector."
  ([root]   (mpsXX [] [] [] (list root) []))
  ([parent ks vs q results]
   (if (empty? ks)
     (if (empty? q)
       results
       (let [q0 (peek q)]
         (recur parent (mkeys q0) (mvals q0) (pop q) results)))
     (recur parent (next ks) (next vs) (conj q (first vs)) (conj results (conj parent (first ks)))))))



;; https://gist.github.com/stathissideris/1397681b9c63f09c6992
(defn tree-seq-path
  "Like core's tree-seq but returns a lazy sequence of vectors of the
  paths of the nodes in a tree, via a depth-first walk. It optionally
  applies node-fn to each node before adding it to the path. branch?
  must be a fn of one arg that returns true if passed a node that can
  have children (but may not).  children must be a fn of one arg that
  returns a sequence of the children. Will only be called on nodes for
  which branch? returns true. Root is the root node of the tree."
  [branch? children root & [node-fn]]
  (let [node-fn (or node-fn identity)
        walk (fn walk [path node]
               (let [new-path (conj path (node-fn node))]
                (lazy-seq
                 (cons new-path
                       (when (branch? node)
                         (mapcat (partial walk new-path) (children node)))))))]
    (walk [] root)))




;; My version of paths, but slower
(defn mps1
  ([node] (mps1 [] node []))
  ([path node result]
   (if (associative? node)
     (into result (mapcat #(mps1 (conj path %) %2 []) (mkeys node) (mvals node)))
     (conj result path))))


;; not bad, but not quite good enough
;; also very similar to AD paths
(defn mps
  ([node] (mps [] node))
  ([path node]
   (cond (map? node) (mapcat #(mps (conj path %) %2) (keys node) (vals node))
       (vector? node) (mapcat #(mps (conj path %) %2) (range) node)
       :else [path])))


(defn xmps
  ([node] (xmps [] node))
  ([path node]
   (cond (map? node)
         (transduce (mapcat (fn [[k v]] (xmps (conj path k) v))) conj [] node)
       (vector? node)
         (transduce (comp (map-indexed (fn [k v] (xmps (conj path k) v))) cat) conj [] node)
       :else [path])))

(defn xlps
  ([node] (xlps [] node))
  ([path node]
   (cond (map? node)
         (sequence (mapcat (fn [[k v]] (xlps (conj path k) v))) node)
       (vector? node)
         (sequence (comp (map-indexed (fn [k v] (xlps (conj path k) v))) cat) node)
       :else [path])))


(require '[clojure.core.reducers :as r])
;; reducers make more sense with larger data

;; Works but still slower
(defn rmps
  ([node] (rmps [] node))
  ([path node]
   (cond (map? node) (into [] (r/mapcat (fn [[k v]] (rmps (conj path k) v)) node))
       (vector? node) (into [] (r/mapcat (fn [[k v]] (rmps (conj path k) v))
                                         (map vector (range) node)))
       :else [path])))


;; works but not fast enough
#_ (defn rmps
  ([node] (into [] (rmps [] node)))
  ([path node]
   (cond (map? node) (r/mapcat (fn [[k v]] (rmps (conj path k) v)) node)
       (vector? node)  (r/mapcat (fn [[k v]] (rmps (conj path k) v))
                                         (map vector (range) node))
       :else (r/foldcat [path]))))

;; r/reduce is slightly faster than regular map, but still not enough
(defn rmps
  ([node] (into [] (rmps [] node)))
  ([path node]
   (cond (map? node) (r/mapcat (fn [[k v]] (rmps (conj path k) v)) node)
       (vector? node)  (r/mapcat (fn [[k v]] (rmps (conj path k) v))
                           ;; reduce-kv slightly slower than r/reduce
                           ;; (reduce-kv (fn [res k v] (conj res [k v])) [] node))
                           (r/reduce (fn [res x] (conj res (vector (count res) x))) [] node))
       :else [path])))




;; wrong to (count res) as nested map values will add to count in the recursion

(defn rmps
  ([node] (into [] (rmps [] node)))
  ([path node]
   (cond (map? node) (r/mapcat (fn [[k v]] (rmps (conj path k) v)) node)
       (vector? node)  (r/reduce (fn [res x]
                                      (r/reduce conj res (rmps (conj path (count res)) x)))
                           [] node)
                          
       :else [path])))


