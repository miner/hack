(ns miner.eric2
  (:require [clojure.math.combinatorics :as mc]))


;; ----------------------------------------------------------------------
;; 11/18/19  09:00 by miner -- new problem
;; https://purelyfunctional.tv/issues/purelyfunctional-tv-newsletter-352-tip-use-the-right-kind-of-comment-for-the-job/
;;
;; Write a program that outputs all possibilities to put a + or - or nothing between the
;; numbers 1-9 (in order) such that the result is 100. For example:
;;
;; 1 + 2 + 34 - 5 + 67 - 8 + 9 = 100
;; The function should output a list of strings with these formulas.


;; We need to evaluate all the solutions so no need to be lazy.  The fn-prev-x is like a
;; reducing function, where prev is the previous result (a vector) and x is the new item to
;; be considered.  It should return a new "prev" result or accumuation of numbers.

(defn expand-nums [fn-prev-x coll]
  (letfn [(gen [acc prev coll]
            (if (seq coll)
              (-> acc
                  (gen (conj prev (first coll)) (rest coll))
                  (gen (fn-prev-x prev (first coll)) (rest coll)))
              (conj acc prev)))]
    (when (seq coll)
      (gen () [(first coll)] (rest coll)))))

(defn combine-prev [prev x]
  (conj (pop prev) (+ x (* 10 (peek prev)))))

(defn neg-prev [prev x]
  (conj prev (- x)))

(defn gen-candidates [coll]
  (let [negs #(expand-nums neg-prev %)
        nums #(expand-nums combine-prev %)]
    (into () (mapcat negs) (nums coll))))

(defn eval-candidate [expr]
  (reduce + 0 expr))

(defn print-expr [expr]
  (print (first expr))
  (doseq [x (rest expr)]
    (if (neg? x)
      (print " -" (- x))
      (print " +" x))))

(defn print-solutions []
  (doseq [sol (filter (fn [xs] (= (eval-candidate xs) 100))
                      (gen-candidates (range 1 10)))]
    (print-expr sol)
    (println " = 100")))



;;; END GOOD

;; Seems like there should be something built-in to Clojure to do the expand-nums stuff.
;; Maybe a transducer? With mapcat? Something with tree-seq?



;;; OLD STUFF





(defn gen-candidates3 [coll]
  (let [negs #(expand-nums neg-prev %)
        nums #(expand-nums combine-prev %)]
    (mapcat negs (nums coll))))

(defn gen-exprs1 [coll]
  (mapcat gen-negs (gen-nums coll)))

(defn gen-negs [coll]
  (letfn [(gen [acc prev coll]
            (if (seq coll)
              (-> acc
                  (gen (conj prev (first coll)) (rest coll))
                  (gen (conj prev (- (first coll))) (rest coll)))
              (conj acc prev)))]
    (when (seq coll)
      (gen () [(first coll)] (rest coll)))))
    
(defn gen-nums [coll]
  (letfn [(gen [acc prev coll]
            (if (seq coll)
              (-> acc
                  (gen (conj prev (first coll)) (rest coll))
                  (gen (conj (pop prev) (+ (first coll) (* 10 (peek prev)))) (rest coll)))
              (conj acc prev)))]
    (when (seq coll)
      (gen () [(first coll)] (rest coll)))))



(defn addsubs
  ([coll] (addsubs [(first coll)] (rest coll)))
  ([prev coll]
   (if (seq coll)
     (concat (addsubs (conj prev (first coll)) (rest coll))
             (addsubs (conj prev (- (first coll))) (rest coll)))
     (list prev))))


;; rewrite with acc like keypaths
(defn ncats
  ([coll] (when (seq coll) (ncats [(first coll)] (rest coll))))
  ([prev coll]
   (if (seq coll)
     (concat (ncats (conj prev (first coll)) (rest coll))
             (ncats (conj (pop prev) (+ (first coll) (* 10 (peek prev)))) (rest coll)))
     (list prev))))



(defn gen-cats
  ([coll] (when (seq coll) (gen-cats [] [(first coll)] (rest coll))))
  ([acc prev coll]
   (if (seq coll)
     (-> acc
         (gen-cats (conj prev (first coll)) (rest coll))
         (gen-cats (conj (pop prev) (+ (first coll) (* 10 (peek prev)))) (rest coll)))
     (conj acc prev))))


(defn gen-cs1
  ([coll] (when (seq coll) (persistent! (gen-cs1 (transient []) [(first coll)] (rest coll)))))
  ([acc prev coll]
   (if (seq coll)
     (-> acc
         (gen-cs1 (conj prev (first coll)) (rest coll))
         (gen-cs1 (conj (pop prev) (+ (first coll) (* 10 (peek prev)))) (rest coll)))
     (conj! acc prev))))



;; almost good enough but still pretty slow
(defn ncp [coll]
  (let [vv (vec coll)]
    (into []
          (comp
           (map (fn [ss]
                  (reduce (fn [v i]
                     (assoc (update v i + (* 10 (v (dec i))))
                            (dec i) 0))
                   vv
                   ss)))
                (map #(remove zero? %)))
         (mc/subsets (range 1 (count vv))))))

(defn print-sols []
  (doseq [sol (filter (fn [xs] (= (apply + xs) 100))
                           (mapcat addsubs (ncp (range 1 10))))]
    (print (first sol))
    (doseq [x (rest sol)]
      (if (neg? x)
        (print " -" (- x))
        (print " +" x)))
    (println " = 100")))






(defn ncp1 [coll]
  (let [vv (vec coll)]
    (map (fn [ss]
           (reduce (fn [v i]
                     (assoc (update v i + (* 10 (v (dec i))))
                            (dec i) nil))
                   vv
                   ss))
         (mc/subsets (range 1 (count vv))))))

(defn ncp2 [coll]
  (let [vv (vec coll)]
    (map #(remove zero? %)
         (map (fn [ss]
                (reduce (fn [v i]
                          (assoc (update v i + (* 10 (v (dec i))))
                                 (dec i) 0))
                        vv
                        ss))
              (mc/subsets (range 1 (count vv)))))))
         


;; mc/partitions returns too many results for our purposes
;; we only want ordered partitions.
;; in a sense mc/subsets could tell us where to join by index

;; very slow
#_
(defn ncs [coll]
  (map (fn [xvs] (mapv #(reduce (fn [r x] (+ x (* 10 r))) 0 %) xvs))
       (filter (fn [parts]
                 (every? #(reduce (fn [a b] (if (= (inc a) b) b (reduced false))) %)
                         parts))
               (mc/partitions coll))))



(defn n100 []
  (filter (fn [xs] (= (apply + xs) 100))
          (mapcat addsubs (ncats (range 1 10)))))






;; not needed below
(comment

(defn oneton [n]
  (mapcat addsubs (ncats (range 1 (inc n)))))

(defn pmc
  ([coll] (pmc [(first coll)] (rest coll)))
  ([prev coll]
   (if (seq coll)
     (mapcat (fn [op] (pmc (conj prev op (first coll)) (rest coll))) '[+ -])
     [prev])))



;; Simply with pos/neg numbers -- never gen operators! Never eval exprs!

(defn pms [colls]
  (mapcat pmc colls))

(defn onen [n]
  (pms (ncats (range 1 (inc n)))))

(defn evalexpr [expr]
  (let [[a op b & more] expr]
    (loop [a a op op b b more more]
      (cond (nil? op) a
            (= op '+) (recur (+ a b) (first more) (second more) (nnext more))
            (= op '-) (recur (- a b) (first more) (second more) (nnext more))
            :else (throw (ex-info "Bad expr" {:expr expr :more more :a a :op op :b b}))))))



)
