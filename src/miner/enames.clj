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




;; submitted version (improved slightly below)   BUT BUGGY with A.B
(defn BUGGY-valid-name92? [s]
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

;; WORKS BUT SLOW
(defn valid-name95? [s]
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
                    (cond (not (:initial? state)) (assoc! state :cap? true :cnt cnt)
                          (= cnt 1) (assoc! state :cap? true :initial? false :cnt cnt)
                          :else (reduced false))

                    (\a \b \c \d \e \f \g \h \i \j \k \l \m \n \o \p \q \r \s \t \u \v \w \x \y \z) 
                    (if (and (:cap? state) (not (:initial? state)))
                      (assoc! state :cnt cnt)
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



;; much faster than -9x
(defn valid-name8? [s]
  (let [step (fn step
               ([] (transient {:cap? false :cnt 0 :initial? false :toks 0}))
               ([state]
                (and state
                     (:cap? state) 
                     (>= (:cnt state) 2)
                     (not (:initial? state))
                     (>= (:toks state) 1)))
               ([state c]
                (let [cnt (inc (:cnt state))
                      input (case  c
                              (\A \B \C \D \E \F \G \H \I \J \K \L \M \N \O \P \Q \R \S \T
                               \U \V \W \X \Y \Z) :cap
                              (\a \b \c \d \e \f \g \h \i \j \k \l \m \n \o \p \q \r \s \t
                               \u \v \w \x \y \z)  :lower
                              \space :space
                              \. :dot)]
                  (case (long cnt)
                    1 (if (= input :cap)
                        (assoc! state :cap? true :initial? false :cnt cnt)
                        (reduced false))
                    2 (case input
                        :dot (cond (:initial? state) (reduced false)
                                   (:cap? state) (assoc! state :initial? true :cnt cnt)
                                   :else (reduced false))
                        :space (reduced false)
                        (assoc! state :cnt cnt))
                    3 (case input
                        :space (if (not (:cap? state))
                                 (reduce false)
                                 (assoc! state :cnt 0 :cap? false :toks (inc (:toks
                                                                              state))))
                        :dot (reduced false)
                        (if (:initial? state)
                          (reduced false)
                          (assoc! state :cnt cnt)))
                    (case input
                      :space (if (or (:initial? state) (not (:cap? state)))
                               (reduced false)
                               (assoc! state :cnt 0 :cap? false :toks (inc (:toks state))))
                      :dot (reduced false)
                      (assoc! state :cnt cnt))))))]
    (transduce identity step s)))




;; it's a bit slower to be careful about strange inputs like punctuation

(defn valid-name83? [s]
  (let [abort (reduced false)
        canonicalize (fn [c]
                       (case c
                         (\A \B \C \D \E \F \G \H \I \J \K \L \M \N \O \P \Q \R \S \T
                          \U \V \W \X \Y \Z)   :cap
                         (\a \b \c \d \e \f \g \h \i \j \k \l \m \n \o \p \q \r \s \t
                          \u \v \w \x \y \z)   :low
                         \. :dot
                         \space :sp
                         nil))
        step (fn step
               ([] (transient {:cap? false :cnt 0 :initial? false :toks 0}))
               ([state]
                (and state
                     (:cap? state) 
                     (>= (:cnt state) 2)
                     (not (:initial? state))
                     (>= (:toks state) 1)))
               ([state c]
                (if-not c
                  abort
                  (let [cnt (inc (:cnt state))]
                    (case (long cnt)
                      1 (if (= c :cap)
                          (assoc! state :cap? true :initial? false :cnt cnt)
                          abort)
                      2 (case c
                          :dot (cond (:initial? state) abort
                                     (:cap? state) (assoc! state :initial? true :cnt cnt)
                                     :else abort)
                          :sp abort
                          (assoc! state :cnt cnt))
                      3 (case c
                          :sp (if (not (:cap? state))
                                abort
                                (assoc! state :cnt 0 :cap? false :toks (inc (:toks
                                                                             state))))
                          :dot abort
                          (if (:initial? state)
                            abort
                            (assoc! state :cnt cnt)))                       
                      (case c
                        :sp (if (or (:initial? state) (not (:cap? state)))
                              abort
                              (assoc! state :cnt 0 :cap? false :toks (inc (:toks state))))
                        :dot abort
                        (assoc! state :cnt cnt)))))))]
    (transduce (map canonicalize) step s)))








;;; SLOWER
(defn valid-name97? [s]
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
                    (if (or (= cnt 1) (not (:initial? state)))
                      (assoc! state :cap? true :initial? false :cnt cnt)
                      (reduced false))

                    (\a \b \c \d \e \f \g \h \i \j \k \l \m \n \o \p \q \r \s \t \u \v \w \x \y \z) 
                    (if (and (:cap? state) (not (:initial? state)))
                      (assoc! state :cnt cnt)
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


(defn valid-name98-SLOW? [s]
  (let [step (fn step
               ([] (transient {:cap? false :cnt 0 :initial? false :toks 0}))
               ([state]
                (and state
                     (not (:fail state))
                     (:cap? state) 
                     (>= (:cnt state) 2)
                     (not (:initial? state))
                     (>= (:toks state) 1)))
               ([state c]
                (if (:fail state)
                  (reduced false)
                (let [cnt (inc (:cnt state))]
                  (case  c
                    (\A \B \C \D \E \F \G \H \I \J \K \L \M \N \O \P \Q \R \S \T \U \V \W \X \Y \Z)
                    (assoc! state :cap? true :initial? false :cnt cnt
                            :fail (and (not= cnt 1) (:initial? state)))


                    (\a \b \c \d \e \f \g \h \i \j \k \l \m \n \o \p \q \r \s \t \u \v \w \x \y \z) 
                    (assoc! state :cnt cnt :fail (or (:initial? state) (not (:cap? state))))

                    \space 
                    (assoc! state :cap? false :cnt 0 :toks (inc (:toks state))
                            :fail (or (not (:cap? state)) (<= cnt 2)))

                    \. 
                    (assoc! state :initial? true :cnt cnt
                            :fail (or (not (:cap? state)) (not= cnt 2)))

                    (reduced false))))))]
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
              "A.B Noway"  ;; bad dot
              "Nope A.c"   ;; bad dot
           ]]
     (assert (not (valid-name? s)) (str "not " s)))
   true))

