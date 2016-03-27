(ns miner.walk
  :require [clojure.walk :as w])

;; SEM: my code is a TOTAL MESS and WRONG TO BOOT

;; Just use a zipper already


;; SEM: I don't like this style with an atom for side-effects.  Should do the walk with in
;; and out params.  Really, should use a zipper which formalizes the "update" in a
;; functional style.

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


(def sample {:a {:bar 1 :foo 2} :b {:foo 3}})

(defn state-foo [coll]
  (let [state (atom [])]
    (w/prewalk
      (fn [x]
        (when-let [foo (and (map? coll) (:foo x))]
          (swap! state conj foo)))
      coll)
    @state))


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

(defn mps
  "Returns the set of paths into a nested map/vector."
  ([root]   (mps [] [] [] (list root) []))
  ([parent ks vs q results]
   (if (empty? ks)
     (if (empty? q)
       results
       (let [q0 (peek q)]
         (recur parent (mkeys q0) (mvals q0) (pop q) results)))
     (recur parent (next ks) (next vs) (conj q (first vs)) (conj results (conj parent (first ks)))))))



