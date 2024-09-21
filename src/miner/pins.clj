(ns miner.pins
  (:require [clojure.data.priority-map :as pm]))

;;; Hacker News: Solving the bowling problem with dynamic programming
;;; https://news.ycombinator.com/item?id=41512129
;;; original   https://simonensemble.github.io/pluto_nbs/bowling.html

;;; problem from MIT 6.06
;;; https://youtu.be/r4-cftqTcdI?si=HIuQK9asc8sn4o8i&t=2220

;;; N bowling pins are arranged tightly along a line.  pin_i {1..N} labeled with
;;; reward r_i (int, could be negative)

;;; bowling ball can score r_i by hitting one pin directly, or hit between two pins, taking
;;; down both, and scoring multiple of (r_i * r_i+1)

;;; Take as many roles as you want.  Leave some pins if you want.  How do you maximize
;;; accumulated score given reward vector.

;;; The article explains dynamic programming approach.  More efficient than my brute force
;;; solution.

;;; For Clojure, we will use zero-based index


(def rewardv [3, 4, -1, 6, -1, 6, 6, 3, -1, -1, 6, -2])
;;; btwn mults [12 -4 -6  -6 -6 36 18 -3   1  -6 -12]
;;; expanded [3 12 4 -4 -1 -6 6 -6 -1 -6 6 36 6 18 3 -3 -1 1 -1 -6 6 -12 -2]


(def exx [3 4 -1 6])

(defn betweenv [rv]
  (mapv * rv (subvec rv 1)))

;; not sure if flat vector or map indexed by position is better representation
(defn expand-reward [rv]
  (conj (vec (interleave rv (map * rv (subvec rv 1)))) (peek rv)))

;; single pins are even indexes, double hits are at odd


;;; pins are vectors of hit 1 or skip 0 per expanded pin (expanded reward count)
(defn all-pin-exps [cnt]
  (assert (odd? cnt))
  ;; every expansion must be two elements
  (let [expand-pin2 (fn [pv]
                      (if (zero? (peek pv))
                        [(conj pv 1 0) (conj pv 0 0) (conj pv 0 1)]
                        [(conj pv 0 0) (conj pv 0 1)]))]
    (loop [pvs [[0] [1]]   i 1]
      (if (= i cnt)
        pvs
        (recur (into [] (mapcat expand-pin2) pvs) (+ i 2))))))


;;; exhaustive
(defn best-way [rv]
  (if (empty? rv)
    [nil 0]
    (let [rexp (expand-reward rv)
          scorefn (fn [pv] (reduce + (mapv * rexp pv)))]
      (reduce (fn [bestv pv]
                (let [score (scorefn pv)]
                  (if (> score (peek bestv))
                    (conj pv score)
                    bestv)))
              [-1]
              (all-pin-exps (count rexp))))))
                  

(defn all-ways [rv]
  (when-not (empty? rv)
    (let [rexp (expand-reward rv)
          scorefn (fn [pv] (reduce + (mapv * rexp pv)))]
      (mapv #(conj % (scorefn %)) (all-pin-exps (count rexp))))))








;;; ----------------------------------------------------------------------


;;; never take negs so replace exp-rew with zeros than you can just add all
;;; all-pins could just take directly from xrew instead of 1/0

;;; much faster (x1000) to prune zero/negs in the expansion and to use actual values instead
;;; of separate hit/skip booleans.

;;; almost embarassing how slow original is.  Probably should delete it so no else sees it.


;;; zeros instead of negs
(defn exp-zero-reward [rv]
  (let [end (peek rv)]
    (conj (mapv (fn [x] (if (neg? x) 0 x)) (interleave rv (map * rv (subvec rv 1))))
          (if (neg? end) 0 end))))

;; not any better
(defn ezreward [rv]
  (let [rv1 (subvec rv 1)]
    (mapv #(max % 0)
          (conj (interleave (map * rv rv1) rv1) (rv 0)))))


(defn all-pin-selections [rexp]
  (assert (odd? (count rexp)))
  ;; every expansion must be two elements
  (let [expand-pin2 (fn [pv]
                      (let [nx (rexp (count pv))
                            nx2 (rexp (inc (count pv)))]
                        (if (zero? (peek pv))
                          (cond (and (zero? nx) (zero? nx2))  [(conj pv 0 0)]
                                (zero? nx)   [(conj pv 0 0) (conj pv 0 nx2)]
                                (zero? nx2)  [(conj pv nx 0) (conj pv 0 0)]
                                :else [(conj pv nx 0) (conj pv 0 0) (conj pv 0 nx2)])
                          (if (zero? nx2)
                            [(conj pv 0 0)]
                            [(conj pv 0 0) (conj pv 0 nx2)]))))
        r0 (rexp 0)
        cnt (count rexp)]
    (loop [pvs (if (zero? r0) [[0]] [[0] [r0]])   i 1]
      (if (= i cnt)
        pvs
        (recur (into [] (mapcat expand-pin2) pvs) (+ i 2))))))


(defn new-way [rv]
  (if (empty? rv)
    [nil 0]
    (let [rexp (exp-zero-reward rv)
          scorefn (fn [pv] (reduce + 0 pv))]
      (reduce (fn [bestv pv]
                (let [score (scorefn pv)]
                  (if (> score (peek bestv))
                    (conj pv score)
                    bestv)))
              [-1]
              (all-pin-selections rexp)))))



;;; integrating sub functions for self-contained solution

(defn my-way1 [rv]
  (let [end (peek rv)
        rexp (conj (mapv (fn [x] (if (neg? x) 0 x)) (interleave rv (map * rv (subvec rv 1))))
                   (if (neg? end) 0 end))
        ;; every expansion must be two elements
        ;; avoid redundant zeros
        expand-pin2 (fn [pv]
                      (let [nx (rexp (count pv))
                            nx2 (rexp (inc (count pv)))]
                        (if (zero? (peek pv))
                          (cond (and (zero? nx) (zero? nx2))  [(conj pv 0 0)]
                                (zero? nx)   [(conj pv 0 0) (conj pv 0 nx2)]
                                (zero? nx2)  [(conj pv nx 0) (conj pv 0 0)]
                                :else [(conj pv nx 0) (conj pv 0 0) (conj pv 0 nx2)])
                          (if (zero? nx2)
                            [(conj pv 0 0)]
                            [(conj pv 0 0) (conj pv 0 nx2)]))))
        r0 (rexp 0)
        cnt (count rexp)]
    (loop [pvs (if (zero? r0) [[0]] [[0] [r0]])   i 1]
      (if (= i cnt)
        (reduce (fn [bestv pv]
                  (let [score (reduce + 0 pv)]
                    (if (> score (peek bestv))
                      (conj pv score)
                      bestv)))
                [-1]
                pvs)
        (recur (into [] (mapcat expand-pin2) pvs) (+ i 2))))))



(defn my-way2 [rv]
  (let [end (peek rv)
        rexp (conj (mapv (fn [x] (if (neg? x) 0 x)) (interleave rv (map * rv (subvec rv 1))))
                   (if (neg? end) 0 end))
        ;; every expansion must be two elements
        ;; avoid redundant zeros
        expand-pin2 (fn [pv]
                      (let [nx (rexp (count pv))
                            nx2 (rexp (inc (count pv)))]
                        (if (zero? (peek pv))
                          (cond (and (zero? nx) (zero? nx2))  [(conj pv 0 0)]
                                (zero? nx)   [(conj pv 0 0) (conj pv 0 nx2)]
                                (zero? nx2)  [(conj pv nx 0) (conj pv 0 0)]
                                :else [(conj pv nx 0) (conj pv 0 0) (conj pv 0 nx2)])
                          (if (zero? nx2)
                            [(conj pv 0 0)]
                            [(conj pv 0 0) (conj pv 0 nx2)]))))
        r0 (rexp 0)]
    (loop [pvs (if (zero? r0) [[0]] [[0] [r0]])   i (dec (count rv))]
      (if (zero? i)
        (reduce (fn [bestv pv]
                  (let [score (reduce + 0 pv)]
                    (if (> score (peek bestv))
                      (conj pv score)
                      bestv)))
                [-1]
                pvs)
        (recur (into [] (mapcat expand-pin2) pvs) (dec i))))))


(defn my-way3 [rv]
  (let [rexp (conj (mapv (fn [x] (if (neg? x) 0 x)) (interleave rv (map * rv (subvec rv 1))))
                   (if (neg? (peek rv)) 0 (peek rv)))
        ;; every expansion must be two elements
        ;; avoid redundant zeros
        expand-pin2 (fn [pv]
                      (let [nx (rexp (count pv))
                            nx2 (rexp (inc (count pv)))]
                        (if (zero? (peek pv))
                          (cond (and (zero? nx) (zero? nx2))  [(conj pv 0 0)]
                                (zero? nx)   [(conj pv 0 0) (conj pv 0 nx2)]
                                (zero? nx2)  [(conj pv nx 0) (conj pv 0 0)]
                                :else [(conj pv nx 0) (conj pv 0 0) (conj pv 0 nx2)])
                          (if (zero? nx2)
                            [(conj pv 0 0)]
                            [(conj pv 0 0) (conj pv 0 nx2)]))))
        r0 (rexp 0)]
    (loop [pvs (if (zero? r0) [[0]] [[0] [r0]])   i (dec (count rv))]
      (if (zero? i)
        (reduce (fn [bestv pv]
                  (let [score (reduce + 0 pv)]
                    (if (> score (peek bestv))
                      (conj pv score)
                      bestv)))
                [-1]
                pvs)
        (recur (into [] (mapcat expand-pin2) pvs) (dec i))))))


;;; not better.  slightly more compact but not any faster
(defn my-way4 [rv]
  (let [rexp (conj (mapv (fn [x] (if (neg? x) 0 x)) (interleave rv (map * rv (subvec rv 1))))
                   (if (neg? (peek rv)) 0 (peek rv)))
        ;; every expansion must be two elements
        ;; avoid redundant zeros
        expand-pin2 (fn [pv]
                      (let [nx (rexp (count pv))
                            nx2 (rexp (inc (count pv)))]
                        (if (zero? (peek pv))
                          (cond-> [(conj pv 0 0)]
                            (pos? nx) (conj (conj pv nx 0))
                            (pos? nx2) (conj (conj pv 0 nx2)))
                          (cond-> [(conj pv 0 0)]
                            (pos? nx2) (conj (conj pv 0 nx2))))))
        r0 (rexp 0)]
    (loop [pvs (if (zero? r0) [[0]] [[0] [r0]])   i (dec (count rv))]
      (if (zero? i)
        (reduce (fn [bestv pv]
                  (let [score (reduce + 0 pv)]
                    (if (> score (peek bestv))
                      (conj pv score)
                      bestv)))
                [-1]
                pvs)
        (recur (into [] (mapcat expand-pin2) pvs) (dec i))))))



;;; lazy version runs slow
(defn my-way31 [rv]
  (let [rexp (conj (mapv (fn [x] (if (neg? x) 0 x)) (interleave rv (map * rv (subvec rv 1))))
                   (if (neg? (peek rv)) 0 (peek rv)))
        ;; every expansion must be two elements
        ;; avoid redundant zeros
        expand-pin2 (fn [pv]
                      (let [nx (rexp (count pv))
                            nx2 (rexp (inc (count pv)))]
                        (if (zero? (peek pv))
                          (cond (and (zero? nx) (zero? nx2))  [(conj pv 0 0)]
                                (zero? nx)   [(conj pv 0 0) (conj pv 0 nx2)]
                                (zero? nx2)  [(conj pv nx 0) (conj pv 0 0)]
                                :else [(conj pv nx 0) (conj pv 0 0) (conj pv 0 nx2)])
                          (if (zero? nx2)
                            [(conj pv 0 0)]
                            [(conj pv 0 0) (conj pv 0 nx2)]))))
        r0 (rexp 0)]
    (loop [pvs (if (zero? r0) [[0]] [[0] [r0]])   i (dec (count rv))]
      (if (zero? i)
        (reduce (fn [bestv pv]
                  (let [score (reduce + 0 pv)]
                    (if (> score (peek bestv))
                      (conj pv score)
                      bestv)))
                [-1]
                pvs)
        (recur (sequence (mapcat expand-pin2) pvs) (dec i))))))





;;; expanded reward does all the multiplications for the double hits.
;;; even indices are values for single hits, odd indices are the doubles.
;;; never take a negative so replace with zero for easy scoring.
;;; basic idea is that following a zero (skip) you can hit one of the next two (not both)
;;; but following a hit, you must skip next which forces a zero.
;;; avoid redundant zeros
;;; This is still mostly exhaustive.  Not the dynamic programming the article used. 

;;; BEST SO FAR

(defn my-way5 [rv]
  (if (empty? rv)
    [nil 0]
    (let [rexp (conj (mapv (fn [x] (if (neg? x) 0 x)) (interleave rv (map * rv (subvec rv 1))))
                     (if (neg? (peek rv)) 0 (peek rv)))
          ;; every expansion must be two elements
          ;; avoid redundant zeros
          expand-pin2 (fn [pv]
                        (let [nx (rexp (count pv))
                              nx2 (rexp (inc (count pv)))
                              pv0 (conj pv 0)
                              pv00 (conj pv0 0)]
                          (if (zero? (peek pv))
                            (cond (and (zero? nx) (zero? nx2))  [pv00]
                                  (zero? nx)   [pv00 (conj pv0 nx2)]
                                  (zero? nx2)  [pv00 (conj pv nx 0)]
                                  :else [pv00 (conj pv nx 0) (conj pv0 nx2)])
                            (if (zero? nx2)
                              [pv00]
                              [pv00 (conj pv0 nx2)]))))
          r0 (rexp 0)]
      (loop [pvs (if (zero? r0) [[0]] [[0] [r0]])   i (dec (count rv))]
        (if (zero? i)
          (reduce (fn [bestv pv]
                    (let [score (reduce + 0 pv)]
                      (if (> score (peek bestv))
                        (conj pv score)
                        bestv)))
                  [-1]
                  pvs)
          (recur (into [] (mapcat expand-pin2) pvs) (dec i)))))))


(defn my-way [rv]
  (if (empty? rv)
    [nil 0]
    (let [rexp (mapv #(if (neg? %) 0 %)
                     (reduce (fn [rexp i] (-> rexp (conj (* (rv i) (rv (dec i)))) (conj (rv i))))
                             [(rv 0)]
                             (range 1 (count rv))))
          ;; every expansion must be two elements
          ;; avoid redundant zeros
          expand-pin2 (fn [pv]
                        (let [nx (rexp (count pv))
                              nx2 (rexp (inc (count pv)))
                              pv0 (conj pv 0)
                              pv00 (conj pv0 0)]
                          (if (zero? (peek pv))
                            (cond (and (zero? nx) (zero? nx2))  [pv00]
                                  (zero? nx)   [pv00 (conj pv0 nx2)]
                                  (zero? nx2)  [pv00 (conj pv nx 0)]
                                  :else [pv00 (conj pv nx 0) (conj pv0 nx2)])
                            (if (zero? nx2)
                              [pv00]
                              [pv00 (conj pv0 nx2)]))))
          r0 (rexp 0)]
      (loop [pvs (if (zero? r0) [[0]] [[0] [r0]])   i (dec (count rv))]
        (if (zero? i)
          (reduce (fn [bestv pv]
                    (let [score (reduce + 0 pv)]
                      (if (> score (peek bestv))
                        (conj pv score)
                        bestv)))
                  [-1]
                  pvs)
          (recur (into [] (mapcat expand-pin2) pvs) (dec i)))))))





(defn zneg [^long x]
  (if (neg? x) 0 x))

;;; much faster -- need to integrate
(defn nezreward [rv]
  (mapv zneg
        (reduce (fn [rexp r] (-> rexp (conj (* r (peek rexp))) (conj r)))
                [(rv 0)]
                (subvec rv 1))))

(defn sem-way [rv]
  (if (empty? rv)
    [nil 0]
    (let [rexp (nezreward rv)
          ;; every expansion must be two elements
          ;; avoid redundant zeros
          expand-pin2 (fn [pv]
                        (let [nx (rexp (count pv))
                              nx2 (rexp (inc (count pv)))
                              pv0 (conj pv 0)
                              pv00 (conj pv0 0)]
                          (if (zero? (peek pv))
                            (cond (and (zero? nx) (zero? nx2))  [pv00]
                                  (zero? nx)   [pv00 (conj pv0 nx2)]
                                  (zero? nx2)  [pv00 (conj pv nx 0)]
                                  :else [pv00 (conj pv nx 0) (conj pv0 nx2)])
                            (if (zero? nx2)
                              [pv00]
                              [pv00 (conj pv0 nx2)]))))
          r0 (rexp 0)]
      (loop [pvs (if (zero? r0) [[0]] [[0] [r0]])   i (dec (count rv))]
        (if (zero? i)
          (reduce (fn [bestv pv]
                    (let [score (reduce + 0 pv)]
                      (if (> score (peek bestv))
                        (conj pv score)
                        bestv)))
                  [-1]
                  pvs)
          (recur (into [] (mapcat expand-pin2) pvs) (dec i)))))))




;; every expansion must be two elements
;; avoid redundant zeros
(defn expand-pinv [rexp pv]
  (let [nx (rexp (count pv))
        nx2 (rexp (inc (count pv)))
        pv0 (conj pv 0)
        pv00 (conj pv0 0)]
    (if (zero? (peek pv))
      (cond (and (zero? nx) (zero? nx2))  [pv00]
            (zero? nx)   [pv00 (conj pv0 nx2)]
            (zero? nx2)  [pv00 (conj pv nx 0)]
            :else [pv00 (conj pv nx 0) (conj pv0 nx2)])
      (if (zero? nx2)
        [pv00]
        [pv00 (conj pv0 nx2)]))))


(defn sem-way2 [rv]
  (if (empty? rv)
    [nil 0]
    (let [rexp (nezreward rv)
          xexpander (mapcat #(expand-pinv rexp %))
          r0 (rexp 0)]
      (loop [pvs (if (zero? r0) [[0]] [[0] [r0]])   i (dec (count rv))]
        (if (zero? i)
          (reduce (fn [bestv pv]
                    (let [score (reduce + 0 pv)]
                      (if (> score (peek bestv))
                        (conj pv score)
                        bestv)))
                  [-1]
                  pvs)
          (recur (into [] xexpander pvs) (dec i)))))))

;;; lazy (sequence xexpander pvs) is slightly slower


;;; not faster, but I think I like the nested reduce instead of the loop
(defn sem-way4 [rv]
  (if (empty? rv)
    [nil 0]
    (let [rexp (nezreward rv)
          xexpander (mapcat #(expand-pinv rexp %))
          r0 (rexp 0)]
      (reduce (fn [bestv pv]
                (let [score (reduce + 0 pv)]
                  (if (> score (peek bestv))
                    (conj pv score)
                    bestv)))
              [-1]
              (reduce (fn [pvs _i] (into [] xexpander pvs))
                      (if (zero? r0) [[0]] [[0] [r0]])
                      (range 1 (count rv)))))))


;; calls (f init) then f on the nested result `n` times.  Zero-th is just init.
;; condensed loop
(defn iterated [n f init]
  (if (pos? n)
    (recur (unchecked-dec n) f (f init))
    init))


;;; iterated instead of forced reduce

(defn sem-way5 [rv]
  (if (empty? rv)
    [nil 0]
    (let [rexp (nezreward rv)
          xexpander (mapcat #(expand-pinv rexp %))
          r0 (rexp 0)]
      (reduce (fn [bestv pv]
                (let [score (reduce + 0 pv)]
                  (if (> score (peek bestv))
                    (conj pv score)
                    bestv)))
              [-1]
              (iterated (dec (count rv))
                        (fn [pvs] (into [] xexpander pvs))
                        (if (zero? r0) [[0]] [[0] [r0]]))))))


