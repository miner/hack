;; http://en.wikipedia.org/wiki/Luhn_algorithm
;; for checking credit card numbers

;; much later blog post, but also covers Luhn
;; https://garajeando.blogspot.com/2017/06/kata-luhn-test-in-clojure.html

;; Good candidate for a spec


(ns miner.luhn
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as g]))




;; official definition, just for testing
(defn x2calc [n]
  {:pre [(<= 0 n 9)]}
  (let [n2 (* 2 n)]
    (if (>= n2 10)
      (+ (mod n2 10) (quot n2 10))
      n2)))

;; notice that the (mod n2 10) is always 1, but we can optimize further...

;; easy enough to just look up
(defn x2g [n]
  {:pre [(<= 0 n 9)]}
  (get [0 2 4 6 8 1 3 5 7 9] n))

;; slightly faster, but not as pretty
(defn x2 [n]
  (case n
    0 0
    1 2
    2 4
    3 6
    4 8
    5 1
    6 3
    7 5
    8 7
    9 9))

    
;; using int is slightly faster than long
(defn digit [ch]
  (- (int ch) (int \0)))

;; slightly faster if digits is a vector, but any seq is OK
(defn checksum-digits [digits]
  ;; pad with a leading 0 to get even count
  (let [digits (if (odd? (count digits)) (cons 0 digits) (seq digits))]
    (mod (+ (reduce + (map x2 (take-nth 2 digits)))
            (reduce + (take-nth 2 (rest digits))))
         10)))

(defn vdigits [num-or-str]
  (mapv digit (str num-or-str)))

;; "num" is an integer or a string of digits
(defn checksum [num]
  (checksum-digits (vdigits num)))

(defn check-digits? [digits]
  (zero? (checksum-digits digits)))

(defn check? [num]
  (check-digits? (vdigits num)))

(defn amex-start? [^String s]
  (and (or (.startsWith s "37") (.startsWith s "34"))
       (== (.length s) 15)))

(defn mastercard-start? [^String s]
  (and (.startsWith s "5") (== (.length s) 16)))

(defn visa-start? [^String s]
  (and (.startsWith s "4")
       (or (= (.length s) 13) (= (.length s) 16))))

;; more on card types and issuers
;; http://en.wikipedia.org/wiki/Bank_card_number
;; http://stackoverflow.com/questions/72768/how-do-you-detect-credit-card-type-based-on-number

;; for now using the CS50x instructions
(defn card-type [num]
  (if-not (check? num)
    :invalid
    (let [s (str num)]
      (cond (amex-start? s) :amex
            (visa-start? s) :visa
            (mastercard-start? s) :mastercard
            :else :valid))))


;; first attempt, but I like the second one better
(defn gen-card1
  ([num-digits] (gen-card1 0 num-digits))
  ([start num-digits]
     (let [bs (if (zero? start) () (map digit (str start)))
           rs (repeatedly (- num-digits (count bs) 1) #(rand-int 10))
           ds (concat bs rs '(0))
           chk (checksum-digits ds)]
       (reduce (fn [acc d] (+ (* 10 acc) d)) 0
               (if (zero? chk) ds (concat bs rs (list (- 10 chk))))))))

;; slightly faster
(defn gen-card
  ([num-digits] (gen-card nil num-digits))
  ([start num-digits]
   (let [bv (if start (mapv digit (str start)) [])
         dvx (into bv (repeatedly (- num-digits (inc (count bv))) #(rand-int 10)))
         dv0 (conj dvx 0)
         chk (checksum-digits dv0)]
     (reduce (fn [acc d] (+ (* 10 acc) d))
             0
             (if (zero? chk) dv0 (conj dvx (- 10 chk)))))))

;; returning a string preserves any leading zeroes
(defn gen-card2
  ([num-digits] (gen-card nil num-digits))
  ([start num-digits]
   (let [bv (if start (mapv digit (str start)) [])
         dvx (into bv (repeatedly (- num-digits (inc (count bv))) #(rand-int 10)))
         dv0 (conj dvx 0)
         chk (checksum-digits dv0)]
     (apply str (if (zero? chk) dv0 (conj dvx (- 10 chk)))))))

;; reduce is slightly faster than  Long/parseLong 


;; had a bug when chk was 0  found with generative testing
        
;; http://www.paypalobjects.com/en_US/vhelp/paypalmanager_help/credit_card_numbers.htm
;; but there's a typo for 76009244561 entry.  I changed it to 76009244567.
(def test-data
  '{6331101999990016 Switch-Solo-Paymentech,
    38520000023237 Diners-Club,
    378282246310005 American-Express,
    5610591081018250 Australian-BankCard,
    6011000990139424 Discover,
    371449635398431 American-Express,
    30569309025904 Diners-Club,
    378734493671000 American-Express-Corporate,
    4111111111111111 Visa,
    4012888888881881 Visa,
    76009244567 Dankort-PBS,
    5019717010103742 Dankort-PBS,
    5105105105105100 MasterCard,
    5555555555554444 MasterCard,
    6011111111111117 Discover,
    3530111333300000 JCB,
    3566002020360505 JCB,
    4222222222222 Visa})



(comment
  (require '[miner.luhn :as lu])

  (every? lu/check? (keys lu/test-data))

  (map #(conj % (lu/card-type (first %))) lu/test-data)
)  


;; https://garajeando.blogspot.com/2017/06/kata-luhn-test-in-clojure.html
;; much slower, but useful for testing

(def sum-digits #(+ (quot % 10) (mod % 10)))

(defn double-when-at-even-position [position num]
  (if (even? (inc position)) (* 2 num) num))

(defn reduce-digits [digits]
  (->> digits
       (reverse)
       (map #(Integer/parseInt (str %)))
       (map-indexed double-when-at-even-position)
       (map sum-digits)
       (apply +)))

(defn valid? [digits]
  (zero? (mod (reduce-digits digits) 10)))

;; to match my check?
(defn gara? [n]
  (valid? (map digit (str n))))

;; refactored from Gara examples
(defn gara-test
  ([] (gara-test gara?))
  ([testfn]
   (and (every? testfn ["00000000000"
                        "00000000505"
                        "00000000018"
                        "00000002030"
                        "00000000091"
                        "49927398716"
                        "79927398713"])
        (not-any? testfn ["00000000001"
                          "49927398712"
                          "79927398715"]))))


(s/def ::credit-card-vector (s/and (s/coll-of int? :into []) check-digits?))

(s/def ::credit-card (s/with-gen check? (fn [] (g/fmap #(gen-card % 15) (g/choose 1 9)))))




