(ns miner.soundex
  (:require [clojure.string :as str]))

;; Clojure challenge: soundex
;; https://purelyfunctional.tv/issues/purelyfunctional-tv-newsletter-337-functional-programming-is-deep/


(defn encode [ch]
  (case ch
    (\b \B \f \F \p \P \v \V) 1
    (\c \C \g \G \j \J \k \K \q \Q \s \S \x \X \z \Z) 2
    (\d \D \t \T) 3
    (\l \L) 4
    (\m \M \n \N) 5
    (\r \R) 6
    (\h \H \w \W) -1
    ;; AEIOUY
    0))

(defn soundex [word]
  (when-let [cs (seq word)]
    (let [c (first cs)
          d (if (pos-int? (encode c)) 1 0)
          sdx (into [(str/upper-case c)]
                    (comp (map encode) (remove neg?) (dedupe)
                          (remove zero?) (drop d) (take 3))
                    cs)]
      (apply str (into sdx (repeat (- 4 (count sdx)) 0))))))


;; ----------------------------------------------------------------------
;; second take, after submission to Eric


(defn xtake
  "Like (take n) transducer, but with optional `pad` item that is provided as necessary if
  input runs out before `n` items are taken."
  ([n] (take n))
  ([n pad]
   (fn [rf]
     (let [nv (volatile! n)]
       (fn
         ([] (rf))
         ([result]
          (let [n @nv]
            (if (zero? n)
              (rf result)
              (rf (unreduced (reduce rf (unreduced result) (repeat n pad)))))))
         ([result input]
          (let [n @nv
                nn (vswap! nv dec)
                result (if (pos? n)
                         (rf result input)
                         result)]
            (if (not (pos? nn))
              (ensure-reduced result)
              result))))))))

(defn upcase [ch]
  (let [c (long ch)]
    (if (<= c (long \Z))
      ch
      (char (+ (long \A) (- c (long \a)))))))

(defn soundex2 [word]
  (when-let [cs (seq word)]
    (let [c (first cs)
          sdx (into [(upcase c)]
                    (comp (map encode) (remove neg?) (dedupe) (remove zero?)
                          (drop (if (pos-int? (encode c)) 1 0)) (xtake 3 0))
                    cs)]
      (apply str sdx))))


(defn smoke-test-soundex
  ([] (smoke-test-soundex soundex))
  ([fsoundex]
  (doseq [[word sdx] {"Soundex",	"S532"
                      "Example",	"E251"
                      "Sownteks",	"S532"
                      "Ekzampul",	"E251"
                      "Euler",	"E460"
                      "Gauss",	"G200"
                      "Hilbert",	"H416"
                      "Knuth",	"K530"
                      "Lloyd",	"L300"
                      "Lukasiewicz",	"L222"
                      "Ellery",	"E460"
                      "Ghosh",	"G200"
                      "Heilbronn",	"H416"
                      "HONEYMAN",  "H555"
                      "Rubin",  "R150"
                      "robert",  "R163"
                      "Rupert",  "R163"
                      "Kant",	"K530"
                      "Ladd",	"L300"
                      "Lissajous",	"L222"
                      "Wheaton",	"W350"
                      "Burroughs",	"B620"
                      "Burrows",	"B620"
                      "O'Hara",	"O600"
                      "Washington",	"W252"
                      "Lee",		"L000"
                      "Gutierrez",	"G362"
                      "Pfister",	"P236"
                      "Jackson",	"J250"
                      "Tymczak",	"T522"
                      "VanDeusen",	"V532"
                      "Ashcroft",	"A261"
                      "Ashcraft",	"A261"}]
    (assert (= (fsoundex word) sdx)  word))
  true))
