(ns miner.mutual)

;; https://en.wikipedia.org/wiki/Mutual_recursion

(declare odd1?)

(defn even1? [n]
  (if (zero? n) true (odd1? (dec n))))

(defn odd1? [n]
  (if (zero? n) false (even1? (dec n))))

(defn even2? [n]
  (letfn [(neven? [n] (if (zero? n) true (nodd? (dec n))))
          (nodd? [n] (if (zero? n) false (neven? (dec n))))]
    (neven? n)))


;; https://en.wikipedia.org/wiki/Hofstadter_sequence#Hofstadter_Female_and_Male_sequences

(defn hf [n]
  (letfn [(f [n] (if (zero? n) 1 (- n (m (f (dec n))))))
          (m [n] (if (zero? n) 0 (- n (f (m (dec n))))))]
    (f n)))

;; Slightly unusual to do it this way with nested defs
(letfn [(f [n] (if (zero? n) 1 (- n (m (f (dec n))))))
        (m [n] (if (zero? n) 0 (- n (f (m (dec n))))))]
  (def hf1 "Hofstadter Female function" f)
  (def hm1 "Hofstadter Male function" m))

;; usual way
(declare hofm)

(defn hoff [n]
  (if (zero? n) 1 (- n (hofm (hoff (dec n))))))

(defn hofm [n]
  (if (zero? n) 0 (- n (hoff (hofm (dec n))))))


(def hf74 [1, 1, 2, 2, 3, 3, 4, 5, 5, 6, 6, 7, 8, 8, 9, 9, 10, 11, 11, 12, 13, 13, 14, 14,
           15, 16, 16, 17, 17, 18, 19, 19, 20, 21, 21, 22, 22, 23, 24, 24, 25, 25, 26, 27,
           27, 28, 29, 29, 30, 30, 31, 32, 32, 33, 34, 34, 35, 35, 36, 37, 37, 38, 38, 39,
           40, 40, 41, 42, 42, 43, 43, 44, 45, 45])


(assert (= hf74 (map hf (range (count hf74)))))

