(ns miner.esymmetry)

;; https://gist.github.com/ericnormand/c888c22c27e0a6c4edec6e83c5069f1a

;; UPDATE from Eric Normand: I have horizontal and vertical switched accidentally, but I won't
;; change it since people have already submitted.


;; submitted
(defn classify [v2]
  (let [horz? (every? (fn [row] (= (seq row) (rseq row))) v2)
        vert? (= (seq v2) (rseq v2))]
    (cond (and horz? vert?) :perfect
          horz? :horizontal
          vert? :vertical
          :else :imperfect)))

;; pretty good for a bit more work
(defn classify2 [v2]
  (let [sym? (fn [v] (let [cnt (count v)
                           half (quot cnt 2)]
                       (= (seq (subvec v 0 half))
                          (rseq (subvec v (- cnt half) cnt)))))
        horz? (every? sym? v2)
        vert? (sym? v2)]
    (cond (and horz? vert?) :perfect
          horz? :horizontal
          vert? :vertical
          :else :imperfect)))

;; fastest with loop indexing
(defn classify4 [v2]
  (let [sym? (fn [v]
               (loop [eq true  start 0  end (dec (count v))]
                 (if (and (> end start) eq)
                   (recur (= (v start) (v end)) (inc start) (dec end) )
                   eq)))
        horz? (every? sym? v2)
        vert? (sym? v2)]
    (cond (and horz? vert?) :perfect
          horz? :horizontal
          vert? :vertical
          :else :imperfect)))



;; explicit indexing in reduce, pretty fast
(defn classify5 [v2]
  (let [sym? (fn [v]
               (let [cnt (count v)
                     end (dec cnt)]
                 (or (zero? cnt)
                     (reduce (fn [res ^long i]
                               (if (= (v i) (v (- end i)))
                                 true
                                 (reduced false)))
                             true
                             (range (inc (quot cnt 2)))))))
        horz? (every? sym? v2)
        vert? (sym? v2)]
    (cond (and horz? vert?) :perfect
          horz? :horizontal
          vert? :vertical
          :else :imperfect)))


;; pretty fast but not quite
(defn classify9 [v2]
  (let [sym? (fn [v]
               (let [cnt (count v)
                     half (quot cnt 2)]
                 (= (seq (subvec v 0 half)) (rseq (subvec v (- cnt half) cnt)))))
        horz? (every? sym? v2)
        vert? (sym? v2)]
    (cond (and horz? vert?) :perfect
          horz? :horizontal
          vert? :vertical
          :else :imperfect)))





;; g7s solution slightly faster my initial version
;;  but bug with empty vector.  SEM added (seq) to fix.
(defn symmetric
  [v]
  (let [len (count v)
        mid (quot len 2)]
        (= (seq (subvec v 0 mid))
           (rseq (subvec v (- len mid) len)))))


(defn gclassify
  [rug]
  (case [(symmetric rug) (every? symmetric rug)]
    [true true]   :perfect
    [true false]  :vertical
    [false true]  :horizontal
    [false false] :imperfect))



;;  (let [ydim (count v2)        xdim (count (peek v2))

(def hhh [["a" "b" "a"]
          ["x" "y" "x"]])


(def v1 [["a" "b" "c"]])
(def abba [["a" "b" "b" "a"]])
(def abda [["a" "b" "d" "a"]
           ["a" "d" "d" "a"]])

(def ppp [["a" "b" "a"]
          ["y" "X" "y"]
          ["a" "b" "a"]])

(def imp [["a" "b" "a"]
          ["y" "X" "z"]
          ["z" "b" "a"]])

(def big [[1 1 1 1 1 1 1 1 1 1]
          [1 1 1 1 1 1 1 1 1 1]
          [1 1 1 1 1 1 1 1 1 1]
          [1 1 1 1 1 1 1 1 1 1]
          [1 1 1 1 1 1 1 1 1 1]
          [1 1 1 1 1 1 1 1 1 1]
          [1 1 1 1 1 1 1 1 1 1]
          [1 1 1 1 1 1 1 1 1 1]
          [1 1 1 1 1 1 1 1 1 1]
          [1 1 1 1 1 1 1 1 1 1]])


(def bad [[1 1 1 1 1 1 1 1 1 1]
          [1 1 1 1 1 1 1 1 1 1]
          [1 1 1 1 1 1 1 1 1 1]
          [1 1 1 1 1 1 1 1 1 1]
          [1 1 1 1 1 1 1 1 1 1]
          [1 1 1 1 1 1 1 1 1 1]
          [1 1 1 1 1 1 1 1 1 1]
          [1 1 1 1 1 1 1 1 1 1]
          [1 1 1 1 1 1 1 1 1 1]
          [1 1 1 1 1 1 1 0 1 1]])



(defn smoke-sym
  ([] (smoke-sym classify))
  ([classify]
   (assert (= :perfect (classify [[]])))
   (assert (= :perfect (classify [])))
   (assert (= :perfect (classify [[1]])))
   (assert (= :horizontal (classify hhh)))
   (assert (= :vertical (classify v1)))
   (assert (= :perfect (classify ppp)))
   (assert (= :perfect (classify abba)))
   (assert (= :imperfect (classify abda)))
   (assert (= :imperfect (classify imp)))
   (assert (= :imperfect (classify bad)))
   (assert (= :perfect (classify big)))
   true))

(defn ben
  ([] (ben classify))
  ([& fns]
   (doseq [f fns]
     (println)
     (println (str f))
     (criterium.core/quick-bench (smoke-sym f)))
   (println)))









;;; Actually fast, but too much overhead to have to fix the subvec bug.
;;;;;;;;;;;

(when-not (satisfies?   clojure.core.protocols/IKVReduce (subvec [1] 0))
  (extend-type clojure.lang.APersistentVector$SubVector
    clojure.core.protocols/IKVReduce
    (kv-reduce
      [subv f init]
      (let [cnt (.count subv)]
        (loop [k 0 ret init]
          (if (< k cnt)
            (let [val (.nth subv k)
                  ret (f ret k val)]
              (if (reduced? ret)
                @ret
                (recur (inc k) ret)))
            ret))))))

(defn classify8 [v2]
  (let [sym? (fn [v]
               (let [cnt (count v)
                     off (dec cnt)]
                 (or (zero? cnt)
                     (reduce-kv (fn [res ^long i x]
                                  (if (= x (v (- off i)))
                                    true
                                    (reduced false)))
                                true
                                (subvec v 0 (inc (quot cnt 2)))))))
        horz? (every? sym? v2)
        vert? (sym? v2)]
    (cond (and horz? vert?) :perfect
          horz? :horizontal
          vert? :vertical
          :else :imperfect)))


(defn classify81 [v2]
  (let [sym? (fn [v]
               (let [cnt (count v)
                     off (dec cnt)]
                     (reduce-kv (fn [res ^long i x]
                                  (if (= x (v (- off i)))
                                    true
                                    (reduced false)))
                                true
                                (when (pos? cnt)
                                  (subvec v 0 (inc (quot cnt 2)))))))
        horz? (every? sym? v2)
        vert? (sym? v2)]
    (cond (and horz? vert?) :perfect
          horz? :horizontal
          vert? :vertical
          :else :imperfect)))





;; don't need it
;; (defn transpose [v2] (vec (apply sequence (map vector) v2)))



(defn classify3 [v2]
  (let [sym? (fn [v]
               (let [cnt (count v)
                     end (dec cnt)]
                 (loop [res true i (if (pos? end) (quot cnt 2) -1)]
                   (cond (neg? i) res
                         res (recur (= (v i) (v (- end i))) (dec i))
                         :else false))))
        horz? (every? sym? v2)
        vert? (sym? v2)]
    (cond (and horz? vert?) :perfect
          horz? :horizontal
          vert? :vertical
          :else :imperfect)))

;; slight variations on v3
(defn classify32 [v2]
  (let [sym? (fn [v]
               (let [cnt (count v)
                     end (dec cnt)]
                 (loop [res true i (if (zero? cnt) -1 (quot cnt 2))]
                   (cond (neg? i) res
                         res (recur (= (v i) (v (- end i))) (dec i))
                         :else false))))
        horz? (every? sym? v2)
        vert? (sym? v2)]
    (cond (and horz? vert?) :perfect
          horz? :horizontal
          vert? :vertical
          :else :imperfect)))


(defn classify31 [v2]
  (let [sym? (fn [v]
               (let [cnt (count v)]
                 (or (zero? cnt)
                     (let [off (dec cnt)]
                       (loop [res true i (quot cnt 2)]
                         (cond (neg? i) res
                               res (recur (= (v i) (v (- off i))) (dec i))
                               :else false))))))
        horz? (every? sym? v2)
        vert? (sym? v2)]
    (cond (and horz? vert?) :perfect
          horz? :horizontal
          vert? :vertical
          :else :imperfect)))




#_
(defn same
  ([] nil)
  ([a] a)
  ([a b] (when (= a b) a))
  ([a b & more] (reduce (fn [a b] (if (= a b) a (reduced nil))) (same a b) more)))
