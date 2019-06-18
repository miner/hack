(ns miner.digits)

;; Eric Normand's puzzle from his newsletter
;; https://purelyfunctional.tv/issues/purelyfunctional-tv-newsletter-319-tip-shebang-scripting-with-the-clojure-cli

;; List of digits
;; Write a function that takes a number and returns a sequence of digits. Here's an example:
;; 
;; (digits 3452)
;;=> [3 4 5 2]
;;
;; For extra credit, let digit take another parameter, which is the base of the number in
;; which to represent digits. The default will be 10, but it should work for any number >=
;; 2.
;;
;;(digits 3452 2) ;; 2 means binary
;;=> [1 1 0 1 0 1 1 1 1 1 0 0]



(defn digits1
  ([n] (digits1 n 10))
  ([n radix]
   {:pre [(int? n) (int? radix) (not (neg? n)) (<= Character/MIN_RADIX radix Character/MAX_RADIX)]}
   (mapv (fn [ch]
           ;; Long/toString uses 0-9 and a-z (lowercase)
           (let [c (long ch)]
             (if (>= c (long \a))
               (- c (long \a) -10)
               (- c (long \0)))))
         (Long/toString n radix))))

(defn digits2
  ([n] (digits2 n 10))
  ([n radix]
   {:pre [(int? n) (int? radix) (not (neg? n)) (<= Character/MIN_RADIX radix Character/MAX_RADIX)]}
   (mapv (fn [ch]
           ;; Long/toString uses 0-9 and a-z (lowercase)
           (let [c (long ch)]
             (if (< c (long \a))
               (- c (long \0))
               (- c (long \a) -10))))
           (Long/toString n radix))))


(defn jdigits
  ([n] (jdigits n 10))
  ([n radix]
   {:pre [(int? n) (int? radix) (not (neg? n)) (<= Character/MIN_RADIX radix Character/MAX_RADIX)]}
   (mapv #(Character/digit ^Character % ^Integer radix) (Long/toString n radix))))

(defn digits
  ([n] (digits n 10))
  ([n radix]
   {:pre [(int? n) (int? radix) (not (neg? n)) (>= radix 2)]}
   (loop [n n res ()]
     (if (< n radix)
       (conj res n)
       (recur (quot n radix) (conj res (rem n radix)))))))




;; slow
(defn xdigs
  ([n] (xdigs n 10))
  ([n radix]
   {:pre [(int? n) (int? radix) (not (neg? n)) (>= radix 2)]}
   (into () (comp (drop 1) (take-while #(some pos? %)) (map peek))
         (iterate (juxt #(quot (first %) radix) #(rem (first %) radix)) [n]))))

;; slow
(defn digs
  ([n] (digs n 10))
  ([n radix]
   {:pre [(int? n) (int? radix) (not (neg? n)) (>= radix 2)]}
   (into () (comp (drop 1) (take-while #(some pos? %)) (map peek))
         (iterate (fn [[n r]] [(quot n radix) (rem n radix)]) [n]))))

;; still slowish
(defn ldigs
  ([n] (ldigs n 10))
  ([n radix]
   {:pre [(int? n) (int? radix) (not (neg? n)) (>= radix 2)]}
   (if (zero? n)
     (list 0)
     (nfirst (drop-while (comp pos? peek)
                         (iterate (fn [r] (conj (pop r)
                                                (rem (peek r) radix)
                                                (quot (peek r) radix)))
                                  (list n)))))))
