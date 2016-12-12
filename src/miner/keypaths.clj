(ns miner.keypaths)

;; inspired by
;; http://stackoverflow.com/questions/21768802/how-can-i-get-the-nested-keys-of-a-map-in-clojure

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


(def nested {:a :A, :b :B, :c {:d :D}, :e {:f {:g :G, :h :H} :x {:xx :XX :yy :YY}}})

#_  (keys-in nested)
;; [[:a] [:b] [:c :d] [:e :f :g] [:e :f :h] [:e :x :xx] [:e :x :yy]]



(defn keypaths
  ([m] (keypaths [] m))
  ([prev m]
   (reduce-kv (fn [res k v] (if (map? v)
                              (into res (keypaths (conj prev k) v))
                              (conj res (conj prev k))))
              []
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
(defn kvpaths-all
  ([m] (kvpaths-all [] m))
  ([prev m]
   (reduce-kv (fn [res k v] (if (associative? v)
                              (let [kp (conj prev k)]
                                (conj (into res (kvpaths-all kp v)) kp))
                              (conj res (conj prev k))))
              []
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
