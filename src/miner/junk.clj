




;;; I would like something like reductions but for prewalk and postwalk of nested structures.
;;;  Can tree-seq do this?


(defn postwalk-seq1
   [branch? children root]
   (let [walk (fn walk [node]
                (lazy-seq
                 (if (branch? node)
                   (concat (mapcat walk (children node)) (list node))
                   (list node))))]
     (walk root)))


;; like tree-seq but postwalks the node
(defn postwalk-seq
  ([root] (postwalk-seq sequential? seq root))
  ([branch? children root]
     (let [walk (fn walk [node]
                  (lazy-seq 
                   (if (branch? node)
                     (concat (mapcat walk (children node)) (list node))
                     (list node))))]
       (walk root))))



(defn postwalk-seq-not-so-lazy
  ([root] (postwalk-seq sequential? seq root))
  ([branch? children root]
     (let [walk (fn walk [node]
                  (if (branch? node)
                    (lazy-cat (mapcat walk (children node)) (list node))
                    (list node)))]
       (walk root))))

(defn calls-recur? [form]
  (first (filter (fn [x] (and (seq? x) (= (first x) 'recur)))
                 (tree-seq seq? seq form))))


;; See http://clojuredocs.org/clojure_core/clojure.core/flatten
;; my hacked version is slightly faster than my-flatten
(defn my-flatten1 [l] 
  "free of StackOverflow problem, not lazy and much faster version of flatten."
  (loop [l1 l, l2 []]
    (cond (sequential? (first l1)) (recur (concat (first l1) (rest l1)) l2)
          (empty? l1) (seq l2)
          :else (recur (rest l1) (conj l2 (first l1))))))



;; NOT USED
(defn eager-split= [marker coll]
  (let [marked? (fn [x] (= x marker))]
    (loop [cs coll group [] result []]
      (cond (empty? cs) (conj result group)
            (marked? (first cs)) (recur (rest cs) [] (conj result group))
            :else (recur (rest cs) (conj group (first cs)) result)))))

;; NOT USED
(defn split= [marker coll]
  ;; marker ignored as first or last items
  (let [mark? (fn [x] (= x marker))]
    (take-nth 2 (partition-by mark? (drop-while mark? coll)))))

;; from mailing list
(defn partition-when
  [f coll]
  (lazy-seq
   (when-let [s (seq coll)]
     (let [fst (first s)
           run (cons fst (take-while #(not (f %)) (next s)))]
       (cons run (partition-when f (seq (drop (count run) s))))))))


;; from mailing list
(defn part-when
  [f coll]

  (lazy-seq
   (when-let [s (seq coll)]
     (let [run (take-while #(not (f %)) s)]
       (cons run (part-when f (seq (drop (count run) s))))))))
