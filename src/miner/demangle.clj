
;; replacements for demangle
;; faster but ugly

;; See also clojure.main/demunge


;; horrible loop doing two things at once
(defn ^String demangle2
  "Demangle a clojure identifier name"
  [^String s]
  (let [len (.length s)
        sb (StringBuilder. len)]
    (loop [start 0 under -1 reps demangle-replacements]
      (let [under (if (neg? under) (.indexOf s "_" start) under)]
        (if (neg? under)
          (.toString (.append sb s (int start) (int len)))
          (let [[^String code ^String rep] (first reps)]
            (if (.startsWith s code under)
              (do (.append (.append sb s (int start) (int under)) rep)
                  (recur (long (+ under (.length code)))
                         -1
                         demangle-replacements))
              (recur (long start) (long under) (rest reps)))))))))

;; slightly slower, but a little better
(defn ^String demangle3
  "Demangle a clojure identifier name"
  [^String s]
  (let [len (.length s)
        sb (StringBuilder. len)]
    (loop [start 0]
      (let [under (.indexOf s "_" (int start))]
        (if (neg? under)
          (.toString (.append sb s (int start) (int len)))
          (recur (let [[^String code ^String rep] 
                       (first (filter #(.startsWith s ^String (key %) under)
                                      demangle-replacements))]
                   (.append (.append sb s (int start) (int under)) rep)
                   (long (+ under (.length code))))))))))

;; Fastest
(defn ^String demangle4
  "Demangle a clojure identifier name"
  [^String s]
  (let [len (.length s)
        sb (StringBuilder. len)]
    (loop [start 0]
      (let [under (.indexOf s "_" (int start))]
        (if (neg? under)
          (.toString (.append sb s (int start) (int len)))
          ;; crazy usage of reduce-kv and reduced short-circuit with side-effects
          (recur (long (reduce-kv (fn [_ ^String code ^String rep]
                                    (if (.startsWith s code under)
                                      (do (.append (.append sb s (int start) (int under)) rep)
                                          (reduced (+ under (.length code))))
                                      under))
                                  under
                                  demangle-replacements))))))))
