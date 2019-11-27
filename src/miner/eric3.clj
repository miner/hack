(ns miner.eric3)

;;; 11/23/19  10:09 by miner -- as sent to Eric
;;; 11/25/19  17:12 by miner -- all the solutions:
;;;   https://gist.github.com/ericnormand/a3f661ad5b0868e3cc9e1f3e123f3c3et

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

(defn eval-candidate [xs]
  (reduce + 0 xs))

(defn solutions []
  (let [negs #(expand-nums neg-prev %)
        nums #(expand-nums combine-prev %)
        hundred? #(= (eval-candidate %) 100)]
    (into () (comp (mapcat nums) (mapcat negs) (filter hundred?)) (list (range 1 10)))))

(defn print-solution [xs]
  (print (first xs))
  (doseq [x (rest xs)]
    (if (neg? x)
      (print " -" (- x))
      (print " +" x)))
  (print " = 100"))

(defn print-solutions []
  (doseq [xs (solutions)]
    (print-solution xs)
    (println)))

(defn str-solutions []
  (map (fn [xs]
         (with-out-str
           (print-solution xs)))
       (solutions)))

#_ (pprint (str-solutions))
#_
("1 + 2 + 3 - 4 + 5 + 6 + 78 + 9 = 100"
 "1 + 2 + 34 - 5 + 67 - 8 + 9 = 100"
 "1 + 23 - 4 + 5 + 6 + 78 - 9 = 100"
 "1 + 23 - 4 + 56 + 7 + 8 + 9 = 100"
 "12 + 3 + 4 + 5 - 6 - 7 + 89 = 100"
 "12 - 3 - 4 + 5 - 6 + 7 + 89 = 100"
 "12 + 3 - 4 + 5 + 67 + 8 + 9 = 100"
 "123 - 4 - 5 - 6 - 7 + 8 - 9 = 100"
 "123 + 4 - 5 + 67 - 89 = 100"
 "123 + 45 - 67 + 8 - 9 = 100"
 "123 - 45 - 67 + 89 = 100")

;;;; ---------- submitted above -----------
;;;; other's below




;;; 11/25/19  10:13 by miner -- Eric's solution
;;; SEM added ^long type hints
(defn sum-to [nums target]
  (let [[f & rst] nums]
    (cond
      (not (empty? rst))
      (let [[sec & srst] rst]
        (concat
         (map #(str (Math/abs ^long f) " + " %)
              (sum-to
               rst
               (- target f)))
         (map #(str (Math/abs ^long f) " - " %)
              (sum-to
               (cons (- sec) srst)
               (- target f)))
         (sum-to
          (cons (Long/parseLong (str f sec)) srst)
          target)))

      (= f target)
      [(str (Math/abs ^long f))]

      :else
      [])))

(defn eric []
  (sum-to (range 1 10) 100))



;; My revised version to get rid of string manipulations.
;; This version returns a vector of +/- longs, like my solution.
;; The code is a bit tricky in that it essentially aborts candidates that don't add up right
;; rather than filtering after the fact.  The concat cleans up the empty results.  This uses
;; a lot of memory but is surprisingly fast.

(defn sto [nums target]
  (let [[f & rst] nums]
    (cond (seq rst) (let [[sec & srst] rst]
                      (concat
                       (map #(cons f %) (sto rst (- target f)))
                       (map #(cons f %) (sto (cons (- sec) srst) (- target f)))
                       (sto (cons ((if (neg? f) - +) (* f 10) sec) srst) target)))
          (= f target) (list (list f))
          :else         nil)))

(defn eric1 []
  (sto (range 1 10) 100))

