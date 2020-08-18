(ns miner.epowers)

;; https://gist.github.com/ericnormand/1524630ea3d4cadb84c588a19fccea89

;; Let's say you have an inclusive range of integers [a, b]. You also have an exponent
;; n. What numbers k^n (integer k raised to the nth power) occur in that range? Write a
;; function that takes a, b, and n and returns the numbers k^n.

;; nth-root calculation can be slightly low due to floating-point math
;; loop approach is fastest way I found
(defn powers-in-range [n a b]
  (loop [k (Math/floor (Math/pow a (/ (double n)))) res []]
    (let [kn (long (Math/pow k n))]
      (cond (> kn b) res
            (< kn a) (recur (inc k) res)
            :else (recur (inc k) (conj res kn))))))




;; slower, transducer version
(defn xpowran [n a b]
  (sequence (comp (map #(long (Math/pow % n)))
                  (drop-while #(< % a))
                  (take-while #(<= % b)))
            (iterate inc (Math/floor (Math/pow a (/ (double n)))))))


;; medium performance, readable
(defn powran4 [n a b]
  (for [k (iterate inc (Math/floor (Math/pow a (/ (double n)))))
        :let [kn (long (Math/pow k n))]
        :when (>= kn a)
        :while (<= kn b)]
    kn))



(defn smoke-powers [powers-in-range]
  ;; n = 2, [a, b] = [49, 65]
  (assert (= (powers-in-range 2 49 65) [49 64]))
  ;; that is, 7^2 and 8^2
  ;; n = 3, [a, b] = [1, 27]
  (assert (= (powers-in-range 3 1 27) [1 8 27]))
  ;; 1^3, 2^3, and 3^3
  ;; n = 10, [a, b] = [1, 5]
  (assert (= (powers-in-range 10 1 5) [1]))
  ;; 1^10
  (assert (empty? (powers-in-range 3 28 29)))
  (assert (= (powers-in-range 3 26 73) [27 64]))
  (assert (= (powers-in-range 2 100 1000)
             [100 121 144 169 196 225 256 289 324 361 400 441 484 529 576 625 676
              729 784 841 900 961]))
  true)






;; nth-root is same as pow of inverse, but beware of floating-point errors
(defn nth-root ^double [x n]
  (Math/pow ^double x ^double (/ (double n))))

(defn pow-int [x n]
  (long (Math/pow x n)))

(def nth-root-floor (comp long nth-root))

(defn fn-nth-root-floor [^long n]
  (case n
    0 (constantly 1)
    1 identity
    2 #(long (Math/sqrt %))
    3 #(long (Math/cbrt %))
    #(long (Math/pow ^double % (/ (double n))))))





;; slowish         
;; (halt-when #(> % b) (fn [res x] res)))
;; (range (nth-root-floor a n) (+ (nth-root-floor b n) 2)))))
;; (iterate inc (nth-root-floor a n)))))
;; iter is slightly faster than range




(defn powran1 [n a b]
  (into [] (comp (map #(long (Math/pow % n)))
                 (filter #(<= a % b)))
        (range (nth-root-floor a n) (+ (nth-root-floor b n) 2))))


(defn powran [n a b]
  (let [nroot (fn-nth-root-floor n)]
    (into [] (comp (map #(long (Math/pow % n)))
                   (filter #(<= a % b)))
          (range (nroot a) (+ (nroot b) 2)))))




;; slower
(defn rpow [x n]
  (reduce * 1 (repeat n x)))
