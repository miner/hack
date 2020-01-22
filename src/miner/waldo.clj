(ns miner.waldo)

;;; 01/13/20  10:28 by miner --  Eric's challenge
;; https://purelyfunctional.tv/issues/purelyfunctional-tv-newsletter-360-tip-fold-left-vs-fold-right/

;; You are given a vector of vectors, representing a grid. The grid is filled with
;; values. Your job is to find a given value and return the path into the grid that will get
;; to it.
;;
;; Bonus: Can you make it work with arbitrarily nested vectors of different sizes?

;; two-dimension grid
(defn wheres-waldo [w grid2]
  (first (for [i (range (count grid2)) :let [v (nth grid2 i)]
               j (range (count v)) :when (= (nth v j) w)]
           [i j])))

;; faster two-dimension grid
(defn wheres-waldo2 [w grid2]
  (reduce-kv (fn [_ i v]
               (reduce-kv (fn [_ j x] (when (= w x) (reduced (reduced [i j])))) nil v))
             nil
             grid2))

(defn test-ww
  ([] (test-ww wheres-waldo))
  ([wheres-waldo]
   (let [grid2 [[:A :B :C]
                [:D :E :F]
                [:G :H :I]
                [:J :K :L]
                [:M :N :O]
                [:P :Q :R]
                [:S :T :U]
                [:V :W :X]
                [:Y :and :Z]]]
     (assert (= (wheres-waldo :W grid2) [7 1]))
     (assert (nil? (wheres-waldo :missing grid2))))
   true))


;; arbitrarily nested grid
(defn wheres-waldo4
  ([w nested-vec] (wheres-waldo4 w nested-vec []))
  ([w nested-vec stack]
   (reduce-kv (fn [_ i v]
                (cond (= w v) (reduced (conj stack i))
                      (vector? v) (when-let [r (wheres-waldo4 w v (conj stack i))]
                                    (reduced r))
                      :else nil))
              nil
              nested-vec)))


(defn test-nested
  ([] (test-nested wheres-waldo4))
  ([search-nested]
   (let [grid2 [[:A :B :C]
                [:D :E :F]
                [:G :H :I]
                [:J :K :L]
                [:M :N :O]
                [:P :Q :R]
                [:S :T :U]
                [:V :W :X]
                [:Y :and :Z]]
         grid4 (mapv #(vector :foo [%]) grid2)]
     (assert (= (search-nested :W grid2) [7 1]))
     (assert (= (search-nested :W grid4) [7 1 0 1]))
     (assert (nil? (search-nested :missing grid2))))
   true))




;; Less useful stuff below
;; ----------------------------------------------------------------------

(defn invert2a [grid2]
  (reduce-kv (fn [res i v]
               (reduce-kv (fn [r j x] (assoc r (nth v j) [i j])) res v))
             {}
             grid2))

;; fastest
(defn invert2b [grid2]
  (persistent! (reduce-kv (fn [res i v]
                            (reduce-kv (fn [r j x] (assoc! r (nth v j) [i j])) res v))
                          (transient {})
                          grid2)))



(defn invert2f [grid2]
  (into {} (for [i (range (count grid2)) :let [v (nth grid2 i)]
                 j (range (count v))]
             [(nth v j) [i j]])))


(defn invert2g [grid2]
  (persistent! (reduce (fn [r ij] (assoc! r (get-in grid2 ij) ij))
                       (transient {})
                       (for [i (range (count grid2)) :let [v (nth grid2 i)]
                             j (range (count v))]
                         [i j]))))


(defn test-inv
  ([] (test-inv invert2a))
  ([finvert2]
   (let [grid2 [[:A :B :C]
                [:D :E :F]
                [:G :H :I]
                [:J :K :L]
                [:M :N :O]
                [:P :Q :R]
                [:S :T :U]
                [:V :W :X]
                [:Y :and :Z]]
         inv2 (finvert2 grid2)]
     (assert (= (inv2 :W) [7 1]))
     (assert (nil? (inv2 :missing))))
   true))



;; ----------------------------------------------------------------------

;; depth-first, more general but slower
(defn find-indices
  ([pred nested-vec] (find-indices pred nested-vec []))
  ([pred nested-vec stack]
   (reduce-kv (fn [_ i v]
                (cond (vector? v) (when-let [r (find-indices pred v (conj stack i))]
                                    (reduced r))
                      (pred v) (reduced (conj stack i))
                      :else nil))
              nil
              nested-vec)))

(defn rwaldo [w grid2]
  (find-indices #(= w %) grid2))






(defn wheres-waldo1 [w grid2]
  (reduce-kv (fn [_ i v]
               (when-let [result (reduce-kv (fn [_ j e] (when (= w e) (reduced [i j]))) nil v)]
                 (reduced result)))
             nil
             grid2))





(defn wheres-waldo-OK [w grid2]
  (reduce-kv (fn [r i v]
               (if-let [j (reduce-kv (fn [rr j e] (if (= w e) (reduced j) rr)) nil v)]
                 (reduced [i j])
                 r))
             nil
             grid2))




(defn fwaldo [w grid2]
  (first (for [i (range (count grid2))
               :let [v (nth grid2 i)]
               j (range (count v))
               :when (= (nth v j) w)]
           [i j])))

;; yucky loops, and not faster
(defn lwaldo [w grid2]
  (let [cnt (count grid2)]
    (loop [i 0]
      (when (< i cnt)
        (let [v (nth grid2 i)
              cnt2 (count v)]
          (if-let [res (loop [j 0]
                         (when (< j cnt2)
                           (if (= (nth v j) w)
                             [i j]
                             (recur (inc j)))))]
            res
            (recur (inc i))))))))



;; faster
(defn index-of [x v] 
  (reduce-kv (fn [r i e] (if (= x e) (reduced i) r)) nil v))


;; faster
(defn my-waldo [w grid2]
  (reduce-kv (fn [r i v]
               (if-let [j (index-of w v)] (reduced [i j]) r))
             nil
             grid2))




;; extra junk
(defn ind-of [x v]
  (let [cnt (count v)]
    (loop [i 0]
      (cond (= i cnt) nil
            (= x (v i)) i
            :else (recur (inc i))))))

(defn ki-of [x v]
  (first (keep-indexed (fn [i e] (when (= e x) i)) v)))

;; works
(defn was-waldo [w grid2]
  (first (keep-indexed (fn [i v] (when-let [j (index-of w v)] [i j])) grid2)))


(defn f-index
  ([f v] (f-index f v []))
  ([f v init]
   (reduce-kv (fn [r i e] (if-let [res (f e)] (reduced (conj init i)) r)) nil v)))




;; https://gist.github.com/ericnormand/98a6ecccdbb9bfeeae7fd4bc07284a89
;; Eric's solution (renamed by SEM)
(defn wheres-eric
  ([w world]
   (wheres-eric w world 0))
  ([w world i]
   (cond
    (empty? world)
        nil

    (vector? (first world))
        (let [path (wheres-eric w (first world))]
          (if (nil? path)
            (recur w (rest world) (inc i))
            (vec (cons i path))))

    (= w (first world))
        [i]

    :else
        (recur w (rest world) (inc i)))))


;; Peter Stromberg
(defn wheres-waldo-bonus
  "Find the path to waldo"
  ([waldo vektor]
   (wheres-waldo-bonus waldo vektor []))
  ([waldo vektor path]
   (when (vector? vektor)
     (let [i (.indexOf vektor waldo)]
       (if (> i -1)
         (conj path i)
         (->> (map #(wheres-waldo-bonus waldo %1 (conj path %2)) vektor (range))
              (remove nil?)
              first))))))


;; Chase Lambert (but fixed arg order to fit my test)
(defn wheres-chase [value grid]
  (loop [xs grid
         value value]
    (if (empty? xs)
      nil
      (if ((set (first xs)) value)
        [(.indexOf grid (first xs)) (.indexOf (first xs) value)]
        (recur (rest xs) value)))))


(defn wheres-chase2 [value grid]
  (loop [xs grid
         value value]
    (if (empty? xs)
      nil
      (if ((set (first xs)) value)
        [(.indexOf ^clojure.lang.PersistentVector grid (first xs))
         (.indexOf ^clojure.lang.PersistentVector (first xs) value)]
        (recur (rest xs) value)))))


(defn wheres-chase3 [value grid]
  (loop [xs grid]
    (if (empty? xs)
      nil
      (if ((set (first xs)) value)
        [(index-of grid (first xs))
         (index-of (first xs) value)]
        (recur (rest xs))))))




;; SEM suggestions:
(defn index-of [v w]
  (reduce-kv (fn [_ i x] (when (= x w) (reduced i))) nil v))

(defn wheres-waldo-bonus1
  "Find the path to waldo"
  ([waldo vektor]
   (wheres-waldo-bonus1 waldo vektor []))
  ([waldo vektor path]
   (when (vector? vektor)
     (if-let [i (index-of vektor waldo)]
       (conj path i)
       (first (keep-indexed #(wheres-waldo-bonus1 waldo %2 (conj path %)) vektor))))))
