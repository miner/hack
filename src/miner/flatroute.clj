(ns miner.flatroute)

;; https://www.pixelated-noise.com/blog/2023/02/09/flatten-routes/index.html


(def sample [["/yo" [["/man" :man] ["/manman" :manman]]]
                    ["/foo" [] [[]]
                     ["/bar" :bar]
                     ["/baz" ["/quux" :quux] [["/qux0" :qux0] [["/qux1" :qux1]]]]
                     ["/ba" ["/zz" ["/bazz" :bazz]] ["/baq" :baq]]]])

(defn test-flatr [flatr]
  (assert (= (flatr sample)
          {"/yo/man"         :man
           "/yo/manman"      :manman
           "/foo/bar"        :bar
           "/foo/baz/quux"   :quux
           "/foo/baz/qux0"   :qux0
           "/foo/baz/qux1"   :qux1
           "/foo/ba/zz/bazz" :bazz
           "/foo/ba/baq"     :baq}))
  true)


(defn flat1
  ([nest] (flat1 "" nest))
  ([prefix nest]
   (let [s0 (nth nest 0 nil)
         str0? (string? s0)]
     (cond (and (= (count nest) 2) str0? (keyword? (peek nest)))
               [[(str prefix s0) (peek nest)]]
           str0? (mapcat #(flat1 (str prefix s0) %) (subvec nest 1))
           :else (mapcat #(flat1 prefix %) nest)))))

(defn flatr [nests]
  (into {} (mapcat flat1 nests)))

;; basically same as flatr but using arity for recursion
(defn flatr3
  ([nest] (into {} (flatr3 "" nest)))
  ([prefix nest]
   (let [s0 (nth nest 0 nil)
         str0? (string? s0)]
     (cond (and (= (count nest) 2) str0? (keyword? (peek nest)))
               [[(str prefix s0) (peek nest)]]
           str0? (mapcat #(flatr3 (str prefix s0) %) (subvec nest 1))
           :else (mapcat #(flatr3 prefix %) nest)))))

;; slower and kind of strange merge
(defn flatr4
  ([nest] (into {} (flatr4 "" nest {})))
  ([prefix nest result]
   (let [s0 (nth nest 0 nil)
         str0? (string? s0)]
     (cond (and (= (count nest) 2) str0? (keyword? (peek nest)))
               (assoc result (str prefix s0) (peek nest))
           str0? (apply merge result (mapcat #(flatr4 (str prefix s0) % {}) (subvec nest 1)))
           :else (apply merge result (mapcat #(flatr4 prefix % {}) nest))))))



;; slower but might scale better for deep recursion???  Not pretty
(defn flatr2
  ([nest] (flatr2 nest {}))
  ([nests result]
   (let [kp? (fn [x] (and (vector? x) (keyword? (peek x))))]
     (if (empty? nests)
       result
       (let [nest (first nests)]
         (cond (kp? nest) (recur (rest nests) (assoc result (apply str (pop nest)) (peek nest)))
               (< (count nest) 2) (recur (rest nests) result)
               (string? (first nest))
                   (let [s (first nest)]
                     (recur (into (rest nests)
                                  (mapcat (fn [r]
                                            (cond (kp? r) [(into [s] r)]
                                                  (string? (first r))
                                                      [(into [(str s (first r))] (rest r))]
                                                  (vector? r) (mapv #(into [s] %) r)
                                                  :else nil))
                                          (rest nest)))
                            result))
               ;; plain vector
               :else (recur (into (rest nests) (first nests)) result)))))))


