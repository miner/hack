(ns miner.esegmentations
  (:require [clojure.string :as str]))



(defn append-words [a b]
  (cond
    (empty? a)
    b

    (empty? b)
    a

    :else
    (str a " " b)))

(defn segfirst [s words]
  (for [w words
        :when (clojure.string/starts-with? s w)]
    [w (.substring s (.length w))]))

(defn eric-segment [string words]
  (if (empty? string)
    [""]
    (for [[word rest]  (segfirst string words)
          segmentation (segment rest words)]
      (append-words word segmentation))))
