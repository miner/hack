(ns miner.keypaths)

;; See also similar function in my walk.clj, which I'm now copying into this file.
;; slight difference in required output.
;; I prefer to treat vectors as keyed by index.


;; inspired by
;; http://stackoverflow.com/questions/21768802/how-can-i-get-the-nested-keys-of-a-map-in-clojure


(def sample {:c 42 :a {:bar 1 :foo 2} :b {:bar [3 {:foo 4} 5]}})

(def deep (vec (take 100 (iterate #(hash-map :k %) {:k 0}))))

(def big
  (zipmap [:a :b :c :d :e :f :g :h :i :j :k :l :m :n :o :p :q :r :s :t :u :v :w :x :y :z]
          (repeat [deep sample])))



(def nested {:a :A, :b :B, :c {:d :D}, :e {:f {:g :G, :h :H} :x {:xx :XX :yy :YY}}})



(defn keys-in [m]
  (if (map? m)
    (vec 
     (mapcat (fn [[k v]]
               (let [sub (keys-in v)
                     nested (map #(into [k] %) (filter (comp not empty?) sub))]
                 (if (seq nested)
                   nested
                   [[k]])))
             m))
    []))


#_  (keys-in nested)
;; [[:a] [:b] [:c :d] [:e :f :g] [:e :f :h] [:e :x :xx] [:e :x :yy]]



(defn keypaths1
  ([m] (keypaths1 [] m))
  ([prev m]
   (reduce-kv (fn [res k v] (if (map? v)
                              (into res (keypaths1 (conj prev k) v))
                              (conj res (conj prev k))))
              []
              m)))

;; FASTEST -- avoid copying -- but see keypaths below for slight improvement using associative?
(defn miner49r-keypaths
  ([m] (miner49r-keypaths [] m ()))
  ([prev m result]
   (reduce-kv (fn [res k v] (if (map? v)
                              (miner49r-keypaths (conj prev k) v res)
                              (conj res (conj prev k))))
              result
              m)))


(defn keypaths2
  ([m] (persistent! (keypaths2 [] m (transient []))))
  ([prev m result]
   (reduce-kv (fn [res k v] (if (map? v)
                              (keypaths2 (conj prev k) v res)
                              (conj! res (conj prev k))))
              result
              m)))





;; SEM, but maybe I want to add all the paths, not just the deepest
(defn keypaths-all
  ([m] (keypaths-all [] m))
  ([prev m]
   (reduce-kv (fn [res k v] (if (map? v)
                              (let [kp (conj prev k)]
                                (conj (into res (keypaths-all kp v)) kp))
                              (conj res (conj prev k))))
              []
              m)))

#_  (keypaths-all nested)
;;  [[:a] [:b] [:c :d] [:c] [:e :f :g] [:e :f :h] [:e :f] [:e :x :xx] [:e :x :yy] [:e :x] [:e]]


;; how about one with vector indices?  reduce-kv handles that too

;; all possible kpaths (
(defn kvpaths-all1
  ([m] (kvpaths-all1 [] m))
  ([prev m]
   (reduce-kv (fn [res k v] (if (associative? v)
                              (let [kp (conj prev k)]
                                (conj (into res (kvpaths-all1 kp v)) kp))
                              (conj res (conj prev k))))
              []
              m)))

;; faster to use accumulator as I did in variant in walk.clj
(defn kvpaths-all2
  ([m] (kvpaths-all2 [] m ()))
  ([prev m result]
   (reduce-kv (fn [res k v] (if (associative? v)
                              (let [kp (conj prev k)]
                                (kvpaths-all2 kp v (conj res kp)))
                              (conj res (conj prev k))))
              result
              m)))



;; only the leaf indices
(defn kvpaths
  ([m] (kvpaths [] m))
  ([prev m]
   (reduce-kv (fn [res k v] (if (associative? v)
                              (into res (kvpaths (conj prev k) v))
                              (conj res (conj prev k))))
              []
              m)))

;; Not so good.  Probably better to use clojure.walk
;; Issue: depth-first or breadth-first?
;; Issue:  look at whole nodes, or just leaves of nested structure?

;; depth-first, full node check so pred must be universal
(defn find-key-path
  ([pred data] (if (pred data)
                 []
                 (find-key-path pred [] data)))
  ([pred kpath data]
   (let [res (reduce-kv (fn [kp k v]
                (let [path (conj kp k)
                      found (cond (pred v) path
                                (associative? v) (find-key-path pred path v))]
                  (if found
                    (reduced (reduced found))
                    kp)))
              kpath
              data)]
     (when (reduced? res) @res))))


;; slower but simple
(defn f-key-path [pred data]
   (first (filter #(pred (get-in data %)) (kvpaths data))))

(defn fkp [pred data]
  (loop [kps '([])]
    (when-let [kp (first kps)]
      (let [v (get-in data kp)]
        (cond 
            (pred v) kp
            (map? v) (recur (concat (map #(conj kp %) (keys v)) (rest kps)))
            (vector? v) (recur (concat (map #(conj kp %) (range (count v))) (rest kps)))
          :else (recur (rest kps)))))))



(def players [[1 2 3] [4 5 6] [7 8 9]])


(defn lfkp
  ([pred data] (if (pred data)
                 []
                 (lfkp pred {:kpath []} data)))
  ([pred search-state data]
   (let [{:keys [found more]}
         (reduce-kv (fn [state k v]
                      ;(println "lfkp " state)
                      (let [kpath (:kpath state)
                            path (conj kpath k)]
                        (cond
                            (pred v) (reduced {:found path})
                            (associative? v) (update state :more conj {:kpath path :data v})
                            :else state)))
                    search-state
                    data)]
     (cond found found
         (seq more) (let [{:keys [kpath data]} (first more)]
                      (recur pred {:kpath kpath :more (rest more)} data))))))

;; state in vector [found more kpath], slightly faster than keyword access
(defn vfkp
  ([pred data] (if (pred data)
                 []
                 (vfkp pred [nil nil []] data)))
  ;; search-state [found more kpath]
  ([pred search-state data]
   (let [[found more]
         (reduce-kv (fn [state k v]
                      ;(println "vfkp " state)
                      (let [kpath (peek state)
                            path (conj kpath k)]
                        (cond
                            (pred v) (reduced [path])
                            (associative? v) [nil (conj (nth state 1) [path v]) kpath]
                            :else state)))
                    search-state
                    data)]
     (cond found found
         (seq more) (let [[kpath data] (first more)]
                      (recur pred [nil (rest more) kpath] data))))))


;; only slightly different in using `update` rather than making new state, slightly slower
(defn vfkp2
  ([pred data] (if (pred data)
                 []
                 (vfkp2 pred [nil nil []] data)))
  ;; search-state [found more kpath]
  ([pred search-state data]
   (let [[found more]
         (reduce-kv (fn [state k v]
                      ;(println "vfkp " state)
                      (let [kpath (peek state)
                            path (conj kpath k)]
                        (cond
                            (pred v) (reduced [path])
                            (associative? v) (update state 1 conj [path v])
                            :else state)))
                    search-state
                    data)]
     (cond found found
         (seq more) (let [[kpath data] (first more)]
                      (recur pred [nil (rest more) kpath] data))))))


;; SEM: idea  walk-reduce -- like a reduce that walks subforms



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


;; SEM eager (reduce-kv) is maybe easier to read.  Faster to conj on list.  You really don't
;; care about the order of the generated paths.
(defn rpaths
  "Returns the set of paths into a nested map/vector."
  ([root]   (rpaths [] root))
  ([parent x]
   (if (associative? x)
     (reduce-kv (fn [r k v] (into r (rpaths (conj parent k) v))) () x)
     (list parent))))

;; SEM: we want empty map to return empty list, not nil.  So init return to () instead of
;; nil.
;;
;; SEM: My decision that non-assoc? data should just return nil, rather than throw assertion
;; error.
;;
;; SEM even faster to use an accumulator to avoid some copying.  THIS IS THE FASTEST VERSION

(defn key-paths3
  ([node]
   (when (associative? node)
     (key-paths3 [] node ())))
  ([parent node result]
   (if (associative? node)
     (reduce-kv (fn [r k v] (key-paths3 (conj parent k) v r)) result node)
     (conj result parent))))


;; SEM fastest, best-est.  Slightly reorganized calls to associative?  Maybe slightly better
;; locality for IF.  mpv is associative? -- that is, map? or vector?
(defn keypaths
  ([root]
   (when (associative? root)
     (keypaths [] root ())))
  ([parent mpv result]
   {:pre [(associative? mpv)]}
   ;; essentially, vector or map
   (reduce-kv (fn [r k v]
                (if (associative? v)
                  (keypaths (conj parent k) v r)
                  (conj r (conj parent k))))
              result
              mpv)))


;; Was called depth-first, but "depth" is a misnomer -- maps don't have order, and vector
;; results end up in reverse index order.  Probably not a great idea.

(defn recursive-walk
  ([f root] (recursive-walk f root associative?))
  ([f root node?]
   (when (node? root)
     (recursive-walk f node? [] root ())))
  ([f node? parent node result]
   {:pre [(node? node)]}
   (reduce-kv (fn [r k v]
                (if (node? v)
                  (recursive-walk f node? (conj parent k) v r)
                  (conj r (f (conj parent k) v))))
              result
              node)))



(defn fltn
  ([coll]
   (if (coll? coll)
     (fltn coll [])
     coll))
  ([coll result]
   (reduce (fn [r x]
             (if (coll? x)
               (fltn x r)
               (conj r x)))
           result
           coll)))

;; keypath+value as a conj-ed vector is a way to flatten nested maps into a sequence
;; related to walking the sequence depth-first
;; maps and vectors are structural (nodes to be walked), everything else is a value at that
;; path
;;

(defn kp-flatten [nested]
  (recursive-walk conj nested))



;; for testing
(defn set= [coll coll2] (= (set coll) (set coll2)))


;; As always, beware of micro-benchmarks.  Also, remember that lazy functions look fast in
;; benchmarking but you really need to test something that is fully realized.  This smoke
;; function does enough work, and throws if you get the wrong answer.
(defn smoke [pathf]
  (let [lightly-nested {:a {:aa 1, :ab {:abc 111}}, :b [:b1 :b2 :b3], :c 3}
        solution-set (set [[:a :aa] [:a :ab :abc] [:b 0] [:b 1] [:b 2] [:c]])]
    (assert (= (set (pathf lightly-nested)) solution-set)))
  true)

#_ (quick-bench (smoke paths))

;; Not faster than (vec (repeat ...))
(defn vrepeat [n x]
  (loop [tv (transient []) n ^long n]
    (if (zero? n)
      (persistent! tv)
      (recur (conj! tv x) (dec n)))))


;; SEM: Issue -- creating vector only for missing.  Never extends existing vector.  So
;; sensitive to order of keypaths.

(defn vassoc-in
  "Like assoc-in but creates vectors for levels that do not exist if the key is an integer"
  [m [k & ks] v]
  ;;(println m (vec (cons k ks)) v)
  (if ks
    (assoc m k (vassoc-in (get m k) ks v))
    (cond (or (not (int? k)) (neg? k) (>= k 64) (map? m)) (assoc m k v)
          ;; k must be index
          (nil? m) (assoc (vec (repeat k nil)) k v)
          ;; m must be vector
          (<= k (count m)) (assoc m k v)
          ;; extend m vector as necessary
          :else  (assoc (into m (repeat (- k (count m)) nil)) k v))))


;; Two possible ways of re-encoding maps using keypaths.  As vector tuples [kp val] or as plain
;; vectors [k0 ... kN val].  I decided I don't want the tuple nesting of (vector) keypath
;; and value.  Better to use just one vector.  pop for keypath, peek for value.  Implemented
;; in kp-nest.

#_
(defn reconstruct-in [ksvs]
  ;; ksvs is sequence of [ks v] where ks is a vector keypath as used for assoc-in
  (reduce (fn [r [ks v]]
            (vassoc-in r ks v))
          nil
          ksvs))

;; order matters in that overlapping keypaths can clobber previous values, but that's the
;; same with assoc and merge, etc.

(defn kp-nest [kpvs]
  ;; kpvs is sequence of [k0 ... kn v] where [k0 ... kn] is a vector keypath as used for assoc-in
  (reduce (fn [r kpv]
            (vassoc-in r (pop kpv) (peek kpv)))
          nil
          kpvs))
