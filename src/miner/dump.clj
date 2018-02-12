;; https://gist.github.com/puredanger/bebde3bba60540e834140b0cf2525b09
;; orig by puredanger

(ns miner.dump
  (:require clojure.pprint))


;; SEM revised from clojure.pprint/print-table
;; I want strings to left-format


(defn abs [n]
  (if (neg? n) (- n) n))

(defn first-char [^String s]
  (.charAt s 0))

(defn digit? [^Character ch]
  (Character/isDigit ch))


#_
(defn symbolic? [x]
  (or (symbol? x)
      (keyword? x)))

;; SEM - my version of clojure.pprint/print-table
;; I wanted left-justified symbols, etc.
(defn ptable
  "Prints a collection of maps in a textual table. Prints table headings
   ks, and then a line of output for each row, corresponding to the keys
   in ks. If ks are not specified, use the keys of the first item in rows."
  {:added "1.3"}
  ([ks rows]
   (when (seq rows)
     (let [widths (map
                   (fn [k]
                     (let [w (apply max (count (str k))
                                    (map #(count (str (get % k))) rows))]
                       ;; peek at first row to decide on justification
                       (let [item (get (first rows) k)]
                           (cond (symbol? item) (- w)
                                 (keyword? item) (- w)
                                 (string? item) (if (digit? (first-char item)) w (- w))
                                 :else w))))
                   ks)
           spacers (map #(apply str (repeat (abs %) "-")) widths)
           fmts (map #(str "%" % "s") widths)
           fmt-row (fn [leader divider trailer row]
                     (str leader
                          (apply str (interpose divider
                                                (for [[col fmt] (map vector (map #(get row %) ks) fmts)]
                                                  (format fmt (str col)))))
                          trailer))]
       (println)
       (println (fmt-row "| " " | " " |" (zipmap ks ks)))
       (println (fmt-row "|-" "-+-" "-|" (zipmap ks spacers)))
       (doseq [row rows]
         (println (fmt-row "| " " | " " |" row))))))
  ([rows] (ptable (keys (first rows)) rows)))







;; Prints a table of vars in an ns with name/line/added/macro flag/deprecated flag

(defn pr-vars [ns-sym]
  (->> (ns-publics ns-sym) vals (map meta) (sort-by :name) 
    (map #(select-keys % [:name :line :added :macro :deprecated])) 
    (map #(merge {:added nil :macro '_ :deprecated '_} %)) 
    (ptable [:name :macro :line :added :deprecated])))



