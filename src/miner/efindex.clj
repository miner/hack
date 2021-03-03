(ns miner.efindex)

;; https://gist.github.com/ericnormand/7a5a50b37a6a01b200ca59bd42897c7b

;; filter-index calls a predicate on the indexes of the sequence to decide what to keep


;; warning: nils in coll
(defn standard-filter-index [pred coll]
  {:pre [(not-any? nil? coll)]}
  (keep-indexed (fn [i x] (when (pred i) x)) coll))

;; fails if coll contains nils that you want to keep.  Otherwise, nice.
(defn filter-index1 [pred coll]
  {:pre [(not-any? nil? coll)]}
  (sequence (keep-indexed (fn [i x] (when (pred i) x))) coll))

;; with somewhat ugly fix to allow nils.  I think I would prefer to demand no nils in coll.
(defn filter-index [pred coll]
  (sequence (comp (keep-indexed (fn [i x] (when (pred i) (list x))))
                  cat)
            coll))


;; a tiny bit faster but more obscure so probably not worth it
(defn fin3 [pred coll]
  (sequence (comp (keep-indexed (fn [i x] (when (pred i) (volatile! x))))
                  (map deref))
            coll))

;; reduced is not any better than volatile!


;; cleverish but slow
(defn fin6 [pred coll]
  (sequence (mapcat (fn [i x] (when (pred i) (list x))))
            (range)
            coll))


;; hack for given test example
(defn prime11? [n]
  (contains? #{2 3 5 7 11} n))

(defn smoke-fin
  ([] (smoke-fin filter-index))
  ([filter-index]
   (assert (= (filter-index even? "abcdefg")  '(\a \c \e \g)))
   (assert (= (filter-index prime11? "abcdefghijk") '(\c \d \f \h)))
   (assert (= (filter-index even? [nil nil nil]) [nil nil]))
   true))
