(ns miner.padvec
  (:require [clojure.string :as str]))


;;; original: https://www.emcken.dk/programming/2025/02/01/about-fixing-the-right-problem/
;;; first noticed that pad-vector could be better.  Then decided on a better approach for
;;; sorting the map.

(defn pad-vector
  [v len pad-val]
  (vec (take len (concat v (repeat pad-val)))))

;;; faster assuming v is already a vector
(defn pad-vector2 [v len pad-val]
  (into v (repeat (- len (count v)) pad-val)))

(def sample {:X29 1, :X137 2, :X1108 3, :X137.1 4, :X2.9 5, :X29a 6})

;;; SEM: doesn't preserve original kw
(defn orig-sorter [some-map]
  (->> some-map
       (reduce (fn [acc [k v]]
                 (conj acc [(Float/parseFloat (apply str (rest (name k)))) v]))
               [])
       (into (sorted-map))))


(defn make-sortable
  [n]
  (-> (name n)
      (subs 1) ; removes "K"
      (str/split #"(?=[^\d\.])" 2) ; splits number from non-number
      (pad-vector 2 nil) ; enforce same length for consitent sort
      (update 0 #(Float/parseFloat %))))

(defn second-sorter [some-map]
  (->> some-map
       (into (sorted-map-by #(compare (make-sortable %1)
                                      (make-sortable %2))))))



;;; faster compare, assumes no extra junk after number
(defn make-sortable2 [kw]
  (-> (name kw)
      (subs 1)
      parse-double))

(defn sem-sorter2 [some-map]
  (into (sorted-map-by #(compare (make-sortable2 %1) (make-sortable2 %2)))
        some-map))


;;; allows trailing letters "123a"
(defn make-sortable3 [kw]
  (let [[_ fltstr suffix] (re-matches #"X([\d.]+)(.*)" (name kw))]
    [(parse-double fltstr) suffix]))

       
(defn sem-sorter3 [some-map]
  (into (sorted-map-by #(compare (make-sortable3 %1) (make-sortable3 %2)))
        some-map))


;;; SEM original has lots of extra calls to make-sortable.  Would be better to make one pass
;;; on keys and use cache map.

(defn sem-sorter4 [some-map]
  (let [ks (keys some-map)
        kmap (zipmap ks (map make-sortable3 ks))
        kmcomp #(compare (kmap %1) (kmap %2))]
    (into (sorted-map-by kmcomp) some-map)))

(defn sem-sorter [xkw-map]
  (let [xkeyv (fn [kw] (let [[_ fltstr suffix] (re-matches #"X([\d.]+)(.*)" (name kw))]
                         [(parse-double fltstr) suffix]))
        ks (keys xkw-map)
        kmap (zipmap ks (mapv xkeyv ks))
        kmcomp #(compare (kmap %1) (kmap %2))]
    (into (sorted-map-by kmcomp) xkw-map)))
