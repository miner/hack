(ns miner.anagram)

;; As discussed on Apropos Cast #5 on YouTube.
;; https://youtu.be/DdbVu17AaDQ
;;
;; http://www.4clojure.com/problem/77

;; my solution is basically the same as Mike Fike's
(defn digest [w]
  (sort (seq w)))

(defn anagram-find [words]
  (set (map set (filter next (vals (group-by digest words))))))


;; Eric Normand's version, but slower
;; Note: the (comp vec sort) -- necessary to get a Comparable.  vector is Comparable, but
;; most seqs are not.
(defn ss-find [words]
  (set (map set (filter next (partition-by sort (sort-by (comp vec sort) words))))))

(defn smoke
  ([] (smoke anagram-find))
  ([f]
   (= (f ["meat" "mat" "team" "mate" "eat"])
      #{#{"meat" "team" "mate"}})
   (= (f ["veer" "lake" "item" "kale" "mite" "ever"])
      #{#{"veer" "ever"} #{"lake" "kale"} #{"mite" "item"}})))
  
