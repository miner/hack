(ns miner.efactorize)

;; https://gist.github.com/ericnormand/2d21bd06c44b37cd7ac34137ef46b249

;;; Formatted prime factorization

;;; Prime factorization means representing an integer as a product of primes. A function
;;; that factorizes a number will return a vector of primes, like so: [2 2 3 5]. Your job is
;;; to take such a vector and create a nice string that shows the mathematical notation of
;;; the product.  Use x to indicate multiplication and ^ to indicate exponentiation.

(require '[clojure.string :as str])

(defn format-product [factors]
  (->> factors
       frequencies
       sort
       (map (fn [[f n]] (if (= n 1) f (str f "^" n))))
       (str/join " x ")))
                   
    
(defmacro assert?
  ([pred form result]
   `(do (assert (~pred ~form ~result)) true))
  ([pred form result & more]
   `(and (assert? ~pred ~form ~result)
         (assert? ~pred ~@more))))


(defn smoke-prod [format-product]
  (assert? =
           (format-product [2 2 3 5]) "2^2 x 3 x 5"
           (format-product [2 3 3 3 11 11]) "2 x 3^3 x 11^2"
           (format-product [7])  "7"))



(defn fp1 [factors]
  (apply str
         (sequence (comp (map (fn [[f cnt]] (if (= cnt 1) f (str f "^" cnt))))
                         (interpose " x "))
                   (sort (frequencies factors)))))


(defn fp2 [factors]
  (str/join
         (sequence (comp (map (fn [[f cnt]] (if (= cnt 1) f (str f "^" cnt))))
                         (interpose " x "))
                   (sort (frequencies factors)))))

(defn fp3 [factors]
  (str/join  " x "
         (into [] (map (fn [[f cnt]] (if (= cnt 1) f (str f "^" cnt))))
               (sort (frequencies factors)))))


(defn fp4 [factors]
  (transduce (comp (map (fn [[f cnt]] (if (= cnt 1) f (str f "^" cnt))))
                   (interpose " x "))
             (completing str)
             (sort (frequencies factors))))


(defn fp5 [factors]
  (transduce (comp (map (fn [[f cnt]] (if (= cnt 1) (str f) (str f "^" cnt))))
                   (interpose " x "))
             (completing #(.append ^StringBuilder % ^String %2) str)
             (StringBuilder.)
             (sort (frequencies factors))))



;; My new favorite.  And faster!

;; good as a transducing function,  like having a transient string
(defn string-builder
  ([] (StringBuilder.))
  ([sb] (str sb))
  ([sb x] (.append ^StringBuilder sb (str x))))

(defn xstr [xform coll]
  (transduce xform string-builder coll))

(defn format-product8 [factors]
  (xstr (comp (map (fn [[f cnt]] (if (= cnt 1) f (str f "^" cnt))))
                   (interpose " x "))
        (sort (frequencies factors))))
