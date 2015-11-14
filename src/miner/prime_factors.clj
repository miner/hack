(ns miner.prime-factors)

;; http://stackoverflow.com/questions/9556393/clojure-tail-recursion-with-prime-factors

;; amalloy version, short and lazy
;; (I added type hints)
(defn prime-factors
  ([n] (prime-factors n 2))
  ([^long n ^long candidate]
     (cond (<= n 1) ()
           (zero? (rem n candidate)) (cons candidate (lazy-seq (prime-factors (/ n candidate)
                                                                              candidate)))
           :else (recur n (inc candidate)))))

(defn prime-factorsq
  ([n] (prime-factorsq n 2))
  ([n candidate]
     (cond (<= n 1) ()
           (zero? (rem n candidate)) (cons candidate (lazy-seq (prime-factorsq (quot n candidate)
                                                                              candidate)))
           :else (recur n (inc candidate)))))

;; actually fastest if you do something bigger in a tight loop
(defn prime-factors2 [n]
  (letfn [(step [^long n ^long div]
            (when (< 1 n)
              (let [q (quot n div)]
                (cond
                  (< q div)           (cons n nil)
                  (zero? (rem n div)) (cons div (lazy-step q div))
                  :else               (recur n (inc div))))))
          (lazy-step [n div]
            (lazy-seq
              (step n div)))]
    (lazy-step n 2)))


;; My version fixed
(defn prime-factors1
  ([^long n]
   (cond (<= n 1) ()
         (even? n) (cons 2 (lazy-seq (prime-factors1 (/ n 2))))
         :else (prime-factors1 n 3)))
  ([^long n ^long candidate]
   (cond (<= n 1) ()
         (zero? (rem n candidate))
           (cons candidate (lazy-seq (prime-factors1 (/ n candidate) candidate)))
         :else (recur n (+ 2 candidate)))))

;; handle even (2s) first with single arity
;; 2-arity assumes only odd candidates
;; q < cand implies end (no more factors except for original value)
(defn prime-factors3
  ([^long n]
   (cond (<= n 1) ()
         (even? n) (cons 2 (lazy-seq (prime-factors3 (/ n 2))))
         :else (prime-factors3 n 3)))
  ([^long n ^long candidate]
   (when (> n 2)
     (let [q (quot n candidate)]
       (cond (< q candidate) (list n)
             (zero? (rem n candidate)) (cons candidate (lazy-seq (prime-factors3 q candidate)))
             :else (recur n (+ 2 candidate)))))))

(comment
  (dotimes [_ 100]
    (time (reduce + (map (fn [^long n] (reduce + (prime-factors
                                                  (* 128 2 3 5 13 2 2 23 101 n))))
                         (range 100)))))
  )

  
