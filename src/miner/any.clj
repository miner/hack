(ns miner.any)

;;; https://dev.to/quoll/repetition-gek
;;; Paula Gearon "how to identify duplicates in a seq. For instance, say we have a sequence
;;; of numbers with a duplicate: [0 1 2 3 4 2 6]"



(def xsample (range 100))
(def sample (cons (last xsample) xsample))
(def sample50 (cons 50 xsample))



(defn pg-any= [s]
  (->> s
       rest
       (iterate rest)
       (map #(some #{%1} %2) s)
       (some identity)))

;;; buggy because distinct will have shorter result so you might miss end of s when map cuts
;;; off.  Maybe the last element is the only duplicate.
(defn pg2-any= [s]
  (->> s
       distinct
       (map #(when (not= %1 %2) %1) s)
       (some identity)))

;;  bug
#_  (pg-any= sample)
#_ (pg2-any= [0  0])


(defn miller-any= [[head & tail]]
  (when tail
    (or (some #{head} tail) (recur tail))))

;;; miller is pretty good.  I think the JIT is good at optimizing small recursive functions.

;;; faster than miller because = is faster that testing set inclusion even though it doesn't
;;; look as cool
(defn mx-any= [[head & tail]]
  (when tail
    (or (some #(when (= head %) head) tail) (recur tail))))


;;; slower than miller or mx
(defn any= [s]
  (loop [seen #{} [x & r] s]
    (if (contains? seen x)
      x
      (when r
        (recur (conj seen x) r)))))


(defn rany= [s]
  (let [res (reduce (fn [seen x]
                      (if (contains? seen x)
                        (reduced (reduced x))
                        (conj seen x)))
                    #{}
                    s)]
    (when (reduced? res)
      (deref res))))

(defn sany= [s]
  (first (sequence (comp (map (fn [a b] (when (not= a b) a))) (remove nil?) (take 1))
                   s
                   (concat (distinct s) (repeat nil)))))


(defn fany= [s]
  (first (remove nil? (map (fn [a b] (when (not= a b) a))
                           s
                           (concat (distinct s) (repeat nil))))))
