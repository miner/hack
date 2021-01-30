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

;; assumes exactly one space separator
(defn rtokenize [s]
  (reduce (fn [ts c]
            (if (= c \space)
              (conj ts [])
              (conj (pop ts) (conj (peek ts) c))))
          [[]]
          (seq s)))

;; tolerates mulitple spaces
(defn tokenize [s]
  (sequence (comp (partition-by #(= % \space))
                  (take-nth 2))
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


(defn valid-name2? [s]
  (let [tokens (rtokenize s)]
    (and (every? term? tokens)
         (word? (last tokens))
         (>= (count tokens) 2))))



;; little state machince
;; not so fast, but not bad

(defn valid-name5? [s]
  (let [step (fn [state c]
                (case  c
                  (\A \B \C \D \E \F \G \H \I \J \K \L \M \N \O \P \Q \R \S \T \U \V \W \X \Y \Z) 
                  (-> (assoc state :cap true :init false) (update :cnt inc))

                  (\a \b \c \d \e \f \g \h \i \j \k \l \m \n \o \p \q \r \s \t \u \v \w \x \y \z) 
                  (if (:cap state)
                    (-> (assoc state :init false) (update :cnt inc))
                    (reduced nil))

                  \space 
                  (if (and (:cap state) (>= (:cnt state) 2))
                    (-> (dissoc state :cap) (assoc :cnt 0) (update :toks inc))
                    (reduced nil))

                  \. 
                  (if (and (:cap state) (= (:cnt state) 1))
                    (-> (assoc state :init true) (update :cnt inc))
                    (reduced nil))

                  (reduced nil)))
        res (reduce step {:cap false :cnt 0 :init false :toks 0} (str s " "))]
    (and res
         (not (:init res))
         (>= (:toks res) 2))))


(defn valid-name92? [s]
  (let [step (fn step
               ([] (transient {:cap? false :cnt 0 :initial? false :toks 0}))
               ([state]
                (when-let [res (and state (unreduced (step state \space)))]
                  (and (not (:initial? res))
                       (>= (:toks res) 2))))
               ([state c]
                (let [cnt (inc (:cnt state))]
                  (case  c
                    (\A \B \C \D \E \F \G \H \I \J \K \L \M \N \O \P \Q \R \S \T \U \V \W \X \Y \Z) 
                    (assoc! state :cap? true :initial? false :cnt cnt)

                    (\a \b \c \d \e \f \g \h \i \j \k \l \m \n \o \p \q \r \s \t \u \v \w \x \y \z) 
                    (if (:cap? state)
                      (assoc! state :initial? false :cnt cnt)
                      (reduced false))

                    \space 
                    (if (and (:cap? state) (> cnt 2))
                      (assoc! state :cap? false :cnt 0 :toks (inc (:toks state)))
                      (reduced false))

                    \. 
                    (if (and (:cap? state) (= cnt 2))
                      (assoc! state :initial? true :cnt cnt)
                      (reduced false))

                    (reduced false)))))]
    (transduce identity step s)))


;; It's a little bit faster to inline calcuation of last token.  This is the equivalent of
;; pushing extra space in version-92 above.

;; better version that uses transient state and transduce to process chars.
;; state :cap? = first letter of term is capital, :cnt = count of characters in term,
;; :initial? = term is an initial, :toks = number of tokens

(defn valid-name93? [s]
  (let [step (fn step
               ([] (transient {:cap? false :cnt 0 :initial? false :toks 0}))
               ([state]
                (and state
                     (:cap? state) 
                     (>= (:cnt state) 2)
                     (not (:initial? state))
                     (>= (:toks state) 1)))
               ([state c]
                (let [cnt (inc (:cnt state))]
                  (case  c
                    (\A \B \C \D \E \F \G \H \I \J \K \L \M \N \O \P \Q \R \S \T \U \V \W \X \Y \Z) 
                    (assoc! state :cap? true :initial? false :cnt cnt)

                    (\a \b \c \d \e \f \g \h \i \j \k \l \m \n \o \p \q \r \s \t \u \v \w \x \y \z) 
                    (if (:cap? state)
                      (assoc! state :initial? false :cnt cnt)
                      (reduced false))

                    \space 
                    (if (and (:cap? state) (> cnt 2))
                      (assoc! state :cap? false :cnt 0 :toks (inc (:toks state)))
                      (reduced false))

                    \. 
                    (if (and (:cap? state) (= cnt 2))
                      (assoc! state :initial? true :cnt cnt)
                      (reduced false))

                    (reduced false)))))]
    (transduce identity step s)))





(defn re-valid? [s]
  (some? (re-matches #"(([A-Z][.] )|([A-Z][a-zA-Z]+ ))+([A-Z][a-zA-Z]+)" s)))

;; slightly slower
(defn re-valid2? [s]
  (some? (re-matches #"([A-Z]([.]|([a-zA-Z]+)) )+([A-Z][a-zA-Z]+)" s)))

(defn re-valid3? [s]
  (some? (re-matches #"([A-Z](\.|([a-zA-Z]+)) )+([A-Z][a-zA-Z]+)" s)))


;; solution by steffan-westcott
;; fastest
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
              "Franklin"  ;; must have at least two terms
              "Abe deAnda" ;; word cannot start lower
              "Malcolm X"  ;; needs two+ char term
           ]]
     (assert (not (valid-name? s)) (str "not " s)))
   true))

