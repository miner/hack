(ns miner.reciprocalsum)

;;; https://functional.works-hub.com/learn/a-trivial-mathematical-problem-c5a89

;; For integers N,P,Q: Given N, find all pairs (P, Q) that satisfy the equation:
;;  1/N = 1/P + 1/Q


;; 1/p = 1/q - 1/n
;;  p = qn / (q - n)

;; (p and q are symmetric so you can calc either from the other for given n)

;; obviously, p = 2n, q = 2n will always work
;; a bit of experimentation suggests that you need to test p from n+1 to 2n


;; q and p are symmetric
(defn calcq [n p]
  (/ (* p n) (- p n)))

(defn pqsOK [n]
  (let [guess #(calcq n %)]
    (map (juxt identity guess)
         (remove ratio? (map guess (range (inc n) (inc (* 2 n))))))))

(defn pqs1 [n]
 (let [cq #(calcq n %)]
   (for [p (range (inc n) (inc (* 2 n)))
         :let [q (cq p)]
         :when (int? q)]
     [p q])))


(defn pqs [n]
  (let [cq (fn [p] (let [pn (* p n) d (- p n)]
                     (when (zero? (rem pn d))
                       (quot pn d))))]
   (for [p (range (inc n) (inc (* 2 n)))
         :let [q (cq p)]
         :when q]
     [p q])))

(defn rpqs1 [n]
  (let [cq (fn [p] (let [pn (* p n) d (- p n)]
                     (when (zero? (rem pn d))
                       (quot pn d))))]
    (reduce (fn [r p] (if-let [q (cq p)] (conj r (list p q)) r))
            nil
            (range (* 2 n) n -1))))

(defn rpqs [n]
  (let [cq (fn [p] (let [pn (* p n) d (- p n)]
                     (when (zero? (rem pn d))
                       (quot pn d))))
        n2 (* 2 n)]
    (reduce (fn [r p] (if-let [q (cq p)] (conj r (list p q)) r))
            (list (list n2 n2))
            (range (dec n2) n -1))))

;; good compromise, but not faster
(defn ipqs [n]
  (let [cpq (fn [p] (let [pn (* p n) d (- p n)]
                      (when (zero? (rem pn d))
                        (list p (quot pn d)))))
        n2 (* 2 n)]
    (conj (into [] (keep cpq) (range (inc n) n2))
          (list n2 n2))))


(defn ipqs2 [n]
  (let [cpq (fn [p] (let [pn (* p n) d (- p n)]
                      (when (zero? (rem pn d))
                        (list p (quot pn d)))))
        n2 (* 2 n)]
    (into (list (list n2 n2)) (keep cpq) (range (dec n2) n -1))))

(defn ipqs3 [n]
  (let [cpq (fn [p] (let [pn (* p n) d (- p n)]
                      (when (zero? (rem pn d))
                        (list p (quot pn d)))))
        n2 (* 2 n)]
    (into nil (keep cpq) (range (inc n) (inc n2)))))


(defn kpqs [n]
  (let [cpq (fn [p] (let [pn (* p n) d (- p n)]
                      (when (zero? (rem pn d))
                        (list p (quot pn d)))))]
    (keep cpq (range (inc n) (inc (* n 2))))))



(defn kpqs2 [n]
  (let [cpq (fn [p] (let [pn (* p n) d (- p n)]
                      (when (zero? (rem pn d))
                        (list p (quot pn d)))))
        n2 (* 2 n)]
    (conj (into [] (keep cpq) (range (dec n2) n -1)) [n2 n2])))


(defn rpqs3 [n]
  (let [cq (fn [p] (let [pn (* p n) d (- p n)]
                     (when (zero? (rem pn d))
                       (quot pn d))))
        n2 (* 2 n)]
    (reduce (fn [r p] (if-let [q (cq p)] (conj r (list p q)) r))
            (list (list n2 n2))
            (range (inc n) n2))))

(defn rpqs4 [n]
  (let [cq (fn [p] (let [pn (* p n) d (- p n)]
                     (when (zero? (rem pn d))
                       (quot pn d))))
        n2 (* 2 n)]
    (reduce (fn [r p] (if-let [q (cq p)] (conj r [p q]) r))
            [[n2 n2]]
            (range (inc n) n2))))

;; not faster
(defn tpqs1 [n]
  (let [cpq (fn [p] (let [pn (* p n) d (- p n)]
                     (when (zero? (rem pn d))
                       [p (quot pn d)])))
        n2 (* 2 n)]
    (transduce (keep cpq)
               (completing conj! #(persistent! (conj! % [n2 n2])))
               (transient [])
               (range (inc n) n2))))


(defn tpqs [n]
  (let [cpq (fn [p] (let [pn (* p n) d (- p n)]
                     (when (zero? (rem pn d))
                       (list p (quot pn d)))))
        n2 (* 2 n)]
    (transduce (keep cpq)
               conj
               (list (list n2 n2))
               (range (dec n2) n -1))))


(defn blog-pqs [n]
  (for [p (range (inc n) (inc (* n 2)))
        :let [q (/ (* n p) (- p n))]
        :when (not (ratio? q))]
    [p q]))

(defn assert-npq [n p q]
  (assert (= (/ n) (+ (/ p) (/ q))))
  true)



(defn smoke-pqs [pqs]
  (every? (fn [n] (every? #(apply assert-npq n %) (pqs n))) (range 1 20)))
