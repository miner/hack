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


(defn soundex1 [word]
  (when-let [cs (seq word)]
    (let [c (first cs)
          d (if (pos-int? (encode c)) 1 0)
          sdx (into [(str/upper-case c)]
                    (comp (map encode) (remove neg?) (dedupe)
                          (remove zero?) (drop d) (take 3))
                    cs)
          result (apply str sdx)]
      (if (= (count result) 4)
        result
        (str result (subs "000" 0 (- 4 (count result))))))))



(defn smoke-test-soundex []
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
    (assert (= (soundex word) sdx)  word))
  true)
