(ns miner.enames
  (:require [clojure.string :as str]))

;; https://gist.github.com/ericnormand/23195520ccd38d391d6cbcd907c0ab81

;; Write a function that checks whether a string is a valid name.
;; 
;; A word is a capital letter followed by one or more letters (upper or lower case).
;; 
;; An initial is a single capital letter followed by a period.
;; 
;; A term is either an initial or a word.
;; 
;; A name is a sequence of terms separated by a space. It must have at least 2 terms. The last
;; term must be a word.

(defn tokenize [s]
  (sequence (comp (partition-by #(= % \space))
                  (remove #(= % [\space])))
            s))


(defn capital? [ch]
  (<= (long \A) (long ch) (long \Z)))

(defn lower? [ch]
  (<= (long \a) (long ch) (long \z)))

(defn letter? [ch]
  (or (capital? ch) (lower? ch)))

(defn word? [token]
  (and (capital? (first token))
       (and (next token) (every? letter? (rest token)))))

(defn initial? [token]
  (and (= (count token) 2)
       (capital? (first token))
       (= (second token) \.)))
          
(defn term? [token]
  (or (word? token)
      (initial? token)))

(defn valid-name? [s]
  (let [tokens (tokenize s)]
    (and (every? term? tokens)
         (word? (last tokens))
         (>= (count tokens) 2))))



(defn re-valid? [s]
  (some? (re-matches #"(([A-Z][.] )|([A-Z][a-zA-Z]+ ))+([A-Z][a-zA-Z]+)" s)))

;; slightly slower
(defn re-valid2? [s]
  (some? (re-matches #"([A-Z]([.]|([a-zA-Z]+)) )+([A-Z][a-zA-Z]+)" s)))

(defn sw-name? [s]
  (some? (re-matches #"(\p{IsUppercase}(\.|\p{IsAlphabetic}+) )+\p{IsUppercase}\p{IsAlphabetic}+"
              s)))


(defn smoke-name
  ([] (smoke-name valid-name?))
  ([valid-name?]
   (doseq [s ["George R. R. Martin" "Abraham Lincoln" "J. R. Bob Dobbs" "H. G. Wells"]]
     (assert (valid-name? s) s))
   (doseq [s ["J R Tolkien" ;; no periods
              "J. F. K." ;; must end in word
              "Franklin"]] ;; must have at least two terms
     (assert (not (valid-name? s)) (str "not " s)))
   true))

