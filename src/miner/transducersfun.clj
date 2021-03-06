;; https://gist.github.com/richhickey/b5aefa622180681e1c81

(require '[clojure.core.async :as a])

(def xform (comp (map inc)
              (filter even?)
              (dedupe)
              (flatmap range)
              (partition-all 3)
              (partition-by #(< (apply + %) 7))
              (flatmap flatten)
              (random-sample 1.0)
              (take-nth 1)
              (keep #(when (odd? %) (* % %)))
              (keep-indexed #(when (even? %1) (* %1 %2)))
              (replace {2 "two" 6 "six" 18 "eighteen"})
              (take 11)
              (take-while #(not= 300 %))
              (drop 1)
              (drop-while string?)
              (remove string?)))

(def data (vec (interleave (range 18) (range 20))))

;; lazily transform the data
(sequence xform data)

;; reduce with a transformation (no laziness)
(transduce xform + 0 data)

;;build one collection from a transformation of another, again no laziness
(into [] xform data)

;;create a recipe for a transformation, which can be subsequently sequenced, iterated or reduced
(iteration xform data)

;;transform everything that goes through a channel - same transducer stack!
(let [c (a/chan 1 xform)]
  (a/thread (a/onto-chan c data))
  (a/<!! (a/into [] c)))


;; converts from collection style (->> xs foo bar) into transducer sequence with minimal
;; textual change
(defmacro seq->> [coll & forms]
  `(sequence (comp ~@forms) ~coll))

;; good as a transducing function
(defn string-builder
  ([] (StringBuilder.))
  ([sb] (str sb))
  ([sb x] (.append ^StringBuilder sb (str x))))
