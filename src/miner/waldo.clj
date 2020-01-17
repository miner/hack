(ns miner.waldo)

;;; 01/13/20  10:28 by miner --  Eric's challenge
;; https://purelyfunctional.tv/issues/purelyfunctional-tv-newsletter-360-tip-fold-left-vs-fold-right/

;; You are given a vector of vectors, representing a grid. The grid is filled with
;; values. Your job is to find a given value and return the path into the grid that will get
;; to it.
;;
;; Bonus: Can you make it work with arbitrarily nested vectors of different sizes?


(defn wheres-waldo [w grid2]
  (first (for [i (range (count grid2)) :let [v (nth grid2 i)]
               j (range (count v)) :when (= (nth v j) w)]
           [i j])))

;; faster
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



(defn iwaldo
  ([w nested-vec] (iwaldo w nested-vec []))
  ([w nested-vec stack]
   (reduce-kv (fn [_ i v]
                (cond (vector? v) (when-let [r (iwaldo w v (conj stack i))]
                                    (reduced r))
                      (= w v) (reduced (conj stack i))
                      :else nil))
              nil
              nested-vec)))




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


