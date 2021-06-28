(ns miner.eatbash)

;; https://gist.github.com/ericnormand/4bca050e029545069d0e84b827fc8123

;; Atbash Cipher
;; 
;; The Atbash Cipher is simple: replace every letter with its "mirror" in the alphabet. A is
;; replaced by Z. B is replaced by Y. Etc. Write a function to calculate it. Please maintain
;; capitalization and non-alphabetic characters.

(def uppercase (vec "ABCDEFGHIJKLMNOPQRSTUVWXYZ"))
(def lowercase (vec "abcdefghijklmnopqrstuvwxyz"))
(def atb-cipher-map (merge (zipmap uppercase (rseq uppercase))
                           (zipmap lowercase (rseq lowercase))))


(let [uppercase (vec "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
      lowercase (vec "abcdefghijklmnopqrstuvwxyz")
      atb-cipher-map (merge (zipmap uppercase (rseq uppercase))
                            (zipmap lowercase (rseq lowercase)))
      atb-cipher #(atb-cipher-map % %)]

  (defn atbash [s]
    (transduce (map atb-cipher)
               (fn ([] (StringBuilder.))
                 ([sb] (str sb))
                 ([sb ch] (.append ^StringBuilder sb ^Character ch)))
               s)))


;;; NB: it is faster to cache atb-cipher (or defn it) rather than use the anonymous function
;;; inline.  Not sure if it's recalculating the closure??? Or maybe defeating some JIT
;;; optimization???  Anyway, it's better not to use the bare anonymous fn.


;;; inlined above
(defn atb-cipher [ch]
  (atb-cipher-map ch ch))


(defn atbash1 [s]
  (apply str (map atb-cipher s)))

(defn smoke-at [atbash]
  (assert (= (atbash "") ""))
  (assert (= (atbash "hello") "svool"))
  (assert (= (atbash "Clojure") "Xolqfiv"))
  (assert (= (atbash "Yo!") "Bl!"))
  true)





;;; inlined above
(defn string-builder
  ([] (StringBuilder.))
  ([sb] (str sb))
  ([sb ch] (.append ^StringBuilder sb ^Character ch)))

#_ (doseq [[k v] atb-cipher-map] (println (format "\\%c \\%c" k v)))

(defn atc [c]
  (case c
    \A \Z
    \a \z
    \B \Y
    \b \y
    \C \X
    \c \x
    \D \W
    \d \w
    \E \V
    \e \v
    \F \U
    \f \u
    \G \T
    \g \t
    \H \S
    \h \s
    \I \R
    \i \r
    \J \Q
    \j \q
    \K \P
    \k \p
    \L \O
    \l \o
    \M \N
    \m \n
    \N \M
    \n \m
    \O \L
    \o \l
    \P \K
    \p \k
    \Q \J
    \q \j
    \R \I
    \r \i
    \S \H
    \s \h
    \T \G
    \t \g
    \U \F
    \u \f
    \V \E
    \v \e
    \W \D
    \w \d
    \X \C
    \x \c
    \Y \B
    \y \b
    \Z \A
    \z \a
    c))


;; slightly faster but ugly and error-prone when typing original code.  However, should be
;; better with memory

(defn atbash3 [s]
  (transduce (map atc)
             (fn ([] (StringBuilder.))
               ([sb] (str sb))
               ([sb ch] (.append ^StringBuilder sb ^Character ch)))
             s))




;;; SLOWER
(def A (long \A))
(def M (long \M))
(def N (long \N))
(def Z (long \Z))
(def a (long \a))
(def m (long \m))
(def n (long \n))
(def z (long \z))

(defn atbc [ch]
  (let [c (long ch)]
    (cond (<= A c M) (char (- Z (- c A)))
          (<= N c Z) (char (+ A (- Z c)))
          (<= a c m) (char (- z (- c a)))
          (<= n c z) (char (+ a (- z c)))
          :else ch)))

(defn atbash2 [s]
  (transduce (map atbc)
             (fn ([] (StringBuilder.))
               ([sb] (str sb))
               ([sb ch] (.append ^StringBuilder sb ^Character ch)))
             s))



;; confirm correct letter 
#_  (= (map long lowercase) (take 26 (iterate inc 97)))
#_  (= (map long uppercase) (take 26 (iterate inc 65)))



;; @souenzzo reorganized by SEM

(let [mirror-clusters [[\a \z]
                       [\A \Z]
                       [\0 \9]]
      build-map (fn [[start end]]
                  (let [start (int start)
                        end (int end)]
                    (into {} (map (fn [k v]
                                    [(char k)
                                     (char v)])
                               (range start (inc end))
                               (range end (dec start) -1)))))
      mirror-map (reduce merge (map build-map mirror-clusters))]
  (defn sq-atbash [s]
    (apply str (replace mirror-map s))))


