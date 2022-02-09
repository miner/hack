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

  
;;; How useful is laziness for factors?  Seems like you will always want all the factors so
;;; you might as well be eager.

(defn prime-factors4
  ([^long n]
   (cond (<= n 1) ()
         (even? n) (cons 2 (prime-factors4 (/ n 2)))
         :else (prime-factors4 n 3)))
  ([^long n ^long candidate]
   (when (> n 2)
     (let [q (quot n candidate)]
       (cond (< q candidate) (list n)
             (zero? (rem n candidate)) (cons candidate (prime-factors4 q candidate))
             :else (recur n (+ 2 candidate)))))))


(defn prime1? [n]
  (= 1 (count (prime-factors4 n))))

;; not faster but tail recursive
(defn prime-factors5
  ([^long n]
   (if (<= n 1) [] (prime-factors5 [] n)))
  ([factors ^long n]  
   (if (even? n) (prime-factors5 (conj factors 2) (/ n 2))
       (prime-factors5 factors n 3)))
  ([factors ^long n ^long candidate]
   (if (> n 2)
     (let [q (quot n candidate)]
       (cond (< q candidate) (conj factors n)
             (zero? (rem n candidate)) (prime-factors5 (conj factors candidate) q candidate)
             :else (recur factors n (+ 2 candidate))))
     factors)))
