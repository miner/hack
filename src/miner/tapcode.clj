(ns miner.tapcode
  (:require [clojure.string :as str]))

;; https://en.wikipedia.org/wiki/Tap_code
;; Two dimensional table of alphabet, with c/k in one box.
;; 1-5 taps, separated by short pauses.  Pairs of taps are two-dimensional index into
;; character table. Users have to figure out word breaks, but you could use a longer pause.
;; 
;; The letter "X" is used to break up sentences, and "K" for acknowledgements.
;; So \. is encoded the same as \x, and \k is encode as \c.  That means the encode/decode is
;; not a perfect round trip.

;; one map can go both ways, aka "bijective"
(def tap-map (let [base-map (zipmap (seq "abcdefghijlmnopqrstuvwxyz")
                                    (for [i (range 1 6) j (range 1 6)] [i j]))
                   code-map (reduce-kv (fn [m k v] (assoc m v k)) base-map base-map)]
               ;; K is omitted, use C instead
               (assoc code-map
                      \k (get code-map \c)
                      \. (get code-map \x))))

(defn tap-code [ch]
  (if (and (char? ch) (Character/isUpperCase ^Character ch))
    (get tap-map (Character/toLowerCase ^Character ch))
    ;; otherwise code be vector for tap code
    (get tap-map ch)))

(defn decode-taps [int-coll]
  (str/join (into [] (comp (partition-all 2) (map tap-code)) int-coll)))

(defn encode-str [str]
  (into [] (mapcat tap-code) (seq str)))


(comment
  ;; not as clever
  (merge base-map (zipmap (vals base-map) (keys base-map)))
  ;; better
  (reduce-kv (fn [m k v] (assoc m v k)) base-map base-map)
  )
