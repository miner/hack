(ns miner.hexadecimal
  (:require [clojure.string :as str]
            [clojure.edn :as edn]))

;; original code from:
;; http://exercism.io/submissions/510aa323db734b01abca661447e1bd90

;; Should use upper and lower characters for simpler conversion.  Better than lower-casing
;; input. 

(def ints-by-hex-digit
  (zipmap ["0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "a" "b" "c" "d" "e" "f"]
          [0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15]))

;; Not sure exactly what the expected format for a hex string was.  The (drop 1 ...) suggests that
;; it's "xABC" as opposed to "ABC" or "0xABC" which I would have expected.
;; I'm getting rid of the drop 1.  Use the format of Long/toHexString, string of hex digits.

;; split with #"" is expensive compared to a simple (seq ...), which by the way is what map
;; will do to a string given as the collection argument.  So really, hex-digits is unnecessary.

(defn hex-digits [hex]
  ;;(drop 1 (str/split (str/lower-case hex) #""))
  (str/split (str/lower-case hex) #""))


(def hex-digit->int-digit
  (partial get ints-by-hex-digit))

(defn valid-hex? [hex-digits]
  (every? (complement nil?)
          (map hex-digit->int-digit hex-digits)))

(defn pow [base exp]
  (reduce * (repeat exp base)))

(defn hex-in-position->int
  [position hex-digit]
  (* (hex-digit->int-digit hex-digit)
     (pow 16 position)))

(defn reversed-hex-digits->int
  [reversed-hex-digits]
  (if (valid-hex? reversed-hex-digits)
    (reduce + (map-indexed hex-in-position->int
                           reversed-hex-digits))
    0))

(defn hex-to-int [hex]
  (->> hex
       hex-digits
       reverse
       reversed-hex-digits->int))


;; SEM confirmation
(defn test-hex-fn [f]
  (every? (fn [n] (= n (f (Long/toHexString n)))) (range 1000)))

;; cheating but good for testing
(defn read-hex [hex]
  (edn/read-string (str "0x" hex)))


(def hexchar (merge (zipmap "0123456789ABCDEF" (range 16))
                    (zipmap "abcdef" (range 10 16))))

(defn throw-if-invalid [hex]
  (when-let [invalid (first (filter (complement hexchar) hex))]
    (throw (ex-info "Invalid hexadecimal"
                    {:hex hex
                     :invalid-hex-digit invalid}))))

(defn hex->long [hex]
  (reduce (fn [n ch] (if-let [d (hexchar ch)] (+ (* 16 n) d) (reduced nil))) 0 hex))

(defn safe-hex->long [hex]
  (or (hex->long hex)
      (throw-if-invalid hex)))

