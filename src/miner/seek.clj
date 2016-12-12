;; http://dev.clojure.org/jira/browse/CLJ-2056

;; suggested patch
(defn seek
  "Returns first item from coll for which (pred item) returns true.
   Returns nil if no such item is present, or the not-found value if supplied."
  {:added "1.9"
   :static true}
  ([pred coll] (seek pred coll nil))
  ([pred coll not-found]
    (reduce (fn [_ x]
              (if (pred x)
                (reduced x)
                not-found))
              not-found coll)))

;; other seek name for `seek`
;; find-first
;; filter1
;; 


;; my idea: might also wanted the position (nth) of item

;; probably fast, but not pretty
(defn position [pred coll] 
  (let [index (reduce (fn [i x]
                         (if (pred x)
                           (reduced (reduced i))
                           (inc i)))
                       0 coll)]
     (when (reduced? index) @index)))


;; pretty but laziness not necessarily useful
(defn indices-where [pred coll]
  (sequence (keep-indexed (fn [i x] (when (pred x) i))) coll))

(defn index-where [pred coll]
  (first (indices-when pred coll)))

(defn where
  ([pred coll] (where pred 0 coll))
  ([pred start coll]
   (first (sequence (comp (drop start)
                          (keep-indexed (fn [i x] (when (pred x) (+ i start)))))
                    coll))))

(defn where-all
  ([pred coll]   (keep-indexed (fn [i x] (when (pred x) i)) coll))
  ([pred start coll]   (keep-indexed (fn [i x] (when (pred x) (+ i start))) (drop start coll))))
     



;;;; NO NO NO
;; extension of `find` for maps with predicate instead of literal key
;; vectors are sorta maps of the indices so results are [i x]
;; notice coll comes first, like `find` does
;; NO, THIS ISN'T GOOD
(defn BAD-find-when [coll pred]
  (first (keep-indexed (fn [i x] (when (pred x) [i x])) coll)))




;; a few ugly attempts at variations on position

;; why ffirst? filter returns the input box (not the result) -- try keep instead
(defn mfpos [pred coll] 
  (ffirst (sequence (comp (map (fn [i x] [i (pred x)])) (filter (fn [[i b]] (when b i))))
                   (range)
                   coll)))




(defn spos [pred coll] 
  (first (sequence (comp (map vector) (keep (fn [[i x]] (when (pred x) i))))
                   (range)
                   coll)))


(defn cpos [pred coll] 
  (first (sequence (mapcat (fn [i x] (when (pred x) (list i))))
                   (range)
                   coll)))


(defn mpos [pred coll] 
  (first (sequence (comp (map (fn [i x] (when (pred x) i))) (remove nil?))
                   (range)
                   coll)))

;; Much better, or at least cleaner
(defn kipos [pred coll]
  (first (sequence (keep-indexed (fn [i x] (when (pred x) i))) coll)))

