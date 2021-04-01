(ns miner.euniques)


;; https://gist.github.com/ericnormand/c2a1dd5f9dd04cd0dd9919d464e6fadf
;; Your task is to write a function called  uniques that removes elements that appear twice.

;; Big question:  Does order of results matter?  If so, I got it wrong as the freq map loses
;; order.  As usual, steffan-westcott had it right!


;; inspired by @steffan-westcott
(defn uniques [coll]
  (let [freqs (frequencies coll)]
    (sequence (filter #(= 1 (freqs %))) coll)))


(defn uniques0 [coll]
  (let [sss (into #{} (comp (filter (fn [me] (= (val me) 1)))
                            (map key))
                  (frequencies coll))]
    (sequence (filter sss) coll)))

(defn zuniqs [coll]
  (let [freqs (frequencies coll)]
    (sequence (filter (comp #{1} freqs))
              coll)))


   



;; faster  BUT NOT GUARANTEED ORDER
(defn uniques00 [coll]
  (sequence (comp (filter (fn [me] (= (val me) 1)))
                  (map key))
            (frequencies coll)))





(defmacro assert=
  ([] true)
  ([form result]
   `(do (assert (= ~form ~result)) true))
  ([form result & more]
   `(and (assert= ~form ~result)
         (assert= ~@more))))

;; Decided it's not appropriate for this puzzle
(defn set= [a b]
  (and (= (set a) (set b))
       (= (count a) (count b))))
  

(defmacro assert?
  ([pred form result]
   `(do (assert (~pred ~form ~result)) true))
  ([pred form result & more]
   `(and (assert? ~pred ~form ~result)
         (assert? ~pred ~@more))))



(defn smoke-un [uniques]
  (assert? =
           (uniques []) ()
           (uniques [1 2 3]) '(1 2 3)
           (uniques [1 1 2 3]) '(2 3)
           (uniques [1 2 3 1 2 3]) ()
           (uniques [1 2 3 2]) '(1 3)
           (uniques [0 13 1 13 2 13 3 4 5 6 7 8 9 10 11 12 13]) (range 13)))





;; submissions, similar but a little slower than mine
(defn sw-uniques [xs]
  (let [freqs (frequencies xs)]
    (filter (comp #{1} freqs) xs)))

;; NOT ORDER PRESERVING
(defn nw-uniques
  [coll]
  (->> (frequencies coll)
       (filter #(= 1 (val %)))
       (map key)))

;; cool macro to covert to sequence with transducers
(defmacro seq->> [coll & forms]
  `(sequence (comp ~@forms) ~coll))

;; STILL NOT ORDER PRESERVING
(defn nw-uns [coll]
  (seq->> (frequencies coll)
          (filter #(= 1 (val %)))
          (map key)))



;; slow and complicated
(defn luniqs [coll]
  (loop [seen-at {} sm (sorted-map) cs coll i 0]
    (if (seq cs)
      (let [c (first cs)]
        (if-let [si (get seen-at c)]
          (recur seen-at (dissoc sm si) (rest cs) (inc i))
          (if (contains? seen-at c)
            (recur seen-at sm (rest cs) (inc i))
            (recur (assoc seen-at c i) (assoc sm i c) (rest cs) (inc i)))))
      (sequence (vals sm)))))
