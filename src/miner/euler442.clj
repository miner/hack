(ns miner.euler442)

;;; NOT SOLVED

;;; Eleven-free integers
;;;
;;; An integer is called eleven-free if its decimal expansion does not contain any substring
;;; representing a power of 11 except 1.  For example 3204 and 13431 are eleven-free.  911
;;; and 4121331 are not.  Let E(n) be the nth eleven-free positive integer. (Not zero)
;;; E(3) = 3, E(200) = 213, E(500000) = 531563.   Find E(10^18)


(defn pow11 [limit]
  (take-while #(<= % limit) (iterate #(* % 11) 11)))

(defn subs11fn [limit]
  (let [test11s (sequence (comp (map str) (map re-pattern) (map #(fn [x] (re-find % x))))
                          (pow11 limit))]
    (if (seq test11s)
      (apply some-fn test11s)
      (constantly false))))

(def lazy-elsubs (sequence (map (fn [p] (vector p #(re-find (re-pattern (str p)) %))))
                           (iterate #(* % 11) 11)))

(defn el-free? [n]
  (let [s (str n)]
    (reduce (fn [res [p11 eltest]]
              (cond (> p11 n) (reduced true)
                    (test s) (reduced false)
                    :else res))
            true
            lazy-elsubs)))
             
(defn eleven-pred [n]
  (let [el? (subs11fn n)]
    (fn [i] (el? (str i)))))

(defn eleven-free? [n]
  (let [s (str n)
        eleven? (subs11fn n)]
    (not ((eleven-pred n) n))))

(defn eleven-free1? [n]
  (let [s (str n)
        eleven? (subs11fn n)]
    (not (eleven? s))))

(defn e [n]
  (first (sequence (comp (drop n) (take 1)) (filter eleven-free? (range)))))
      
;;; this is OK for small values, but it is impractical for 10^18        
  
;;; There must be a better mathematical way to calculate the nth eleven-free.  How many do
;;; you drop per power of 11?  Is there some magic for base representation?  Is there a
;;; pattern or factor that matters?

;;; experimenting with base
(defn pr11 [n]
  (with-bindings {#'clojure.pprint/*print-base* 11} (clojure.pprint/write n :stream nil)))

(defn prb [base n]
  (with-bindings {#'clojure.pprint/*print-base* base} (clojure.pprint/write n :stream nil)))


#_
(map (juxt identity #(- % (count (filter eleven-free? (range 1 %)))))
     (range 10000 100000 1000))

;; roughly 20 dropped per 1000 but not exact


(defn ef-cnt [mx]
  (let [rng (range 1 (inc mx))
        efv (filterv eleven-free? rng)]
    (drop (- mx 100)
          (map (juxt identity #(- % (count (take-while (fn [i] (<= i %)) efv))))
               rng))))



(comment 
(e 10)
10

(e 100)
101

(e 1000)
1021

(e 10000)
10313

(e 100000)
104161

(e 1000000)
1052016

;; 1e8
(e 10000000)
10625352


)
