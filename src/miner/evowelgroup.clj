(ns miner.evowelgroup)

;; https://gist.github.com/ericnormand/33d040d1568d1f0253707e42a1cdba85

;; Vowel families

;; Given  two words,  we can  determine if  they have  the same  vowels, ignoring  order and
;; repetition. For instance, "Hello" and "Vowel" both have  \e and \o, so they have the same
;; vowels.  Write a  function that  groups  words into  "families"  that all  have the  same
;; vowels. "Tree" and "tent" also belong to the same family because we ignore the repetition
;; of \e.  For this exercise, the vowels are \a, \e, \i, \o, and \u. If you wish, you may
;; include vowels from other languages and scripts. Your algorithm should not be
;; case-sensitive.

;; Note: the examples return vectors, but it's a bit faster to return seqs so that's what I
;; was going to do.  However, the empty input would naturally return nil so if you want to
;; return a real sequence you have to call `sequence` so you might as well `vec` or
;; equivalent `(mapv val ...)`


(defn vowel-mask [ch]
  (case ch
    (\A \a) 1
    (\E \e) 2
    (\I \i) 4
    (\O \o) 8
    (\U \u) 16
    0))

(defn vbits [word]
  (reduce (fn [score ch] (bit-or score (vowel-mask ch))) 0 word))

(defn vowel-families1 [words]
  (mapv val (group-by vbits words)))


(defn vowel-families [words]
  (let [vowel-mask (fn [ch]
                     (case ch
                       (\A \a) 1
                       (\E \e) 2
                       (\I \i) 4
                       (\O \o) 8
                       (\U \u) 16
                       0))
        vowel-bits (fn [word] (reduce (fn [score ch] (bit-or score (vowel-mask ch))) 0 word))]
    (->> words
         (group-by vowel-bits)
         (mapv val))))



(defn sort= [a b] (= (sort a) (sort b)))

(defn smoke-vow [vowel-families]
  (assert (sort= (vowel-families ["hello" "vowel" "fox" "cot" "hat" "cat"])
          [["hello" "vowel"]
           ["fox" "cot"]
           ["hat" "cat"]]))
  (assert (vowel-families [])  [])
  (assert (sort= (vowel-families ["tree" "tent" "blanket"])
                 [["tree" "tent"] ["blanket"]]))
  (assert (sort= (vowel-families ["a" "an" "the" "e"])
                 [["a" "an"] ["the" "e"]]))
  true)





(defn xvbits [word]
  (transduce (map vowel-mask) (completing bit-or) 0 word))

(defn vfs [words]
  (mapv val (group-by xvbits words)))


;; BUG with map returning nil

;; not as fast but nice and short, and conventional clojure
;; Have to start with #{nil} to match "a" "an".
(defn vfams [words]
  (->> words
       (group-by #(into #{nil} (map {\a \A \e \E \o \O \u \U \A \A \E \E \O \O \U \U}) %))
       (mapv val)))

(defn vfams2 [words]
  (->> words
       (group-by #(transduce (map vowel-mask) (completing bit-or) 0 %))
       (mapv val)))

;; faster
(defn vfams3 [words]
  (->> words
       (group-by #(reduce (fn [score ch] (bit-or score (vowel-mask ch))) 0 %))
       (mapv val)))


(defn vfams31 [words]
  (let [vowel-mask (fn [ch]
                     (case ch
                       (\A \a) 1
                       (\E \e) 2
                       (\I \i) 4
                       (\O \o) 8
                       (\U \u) 16
                       0))
        vowel-bits (fn [word] (reduce (fn [score ch] (bit-or score (vowel-mask ch))) 0 word))]
    (->> words
         (group-by vowel-bits)
         (mapv val))))



;; not as fast as using the `case` approach
(def vbmap {\A 1 \a 1
            \E 2 \e 2
            \I 4\i 4
            \O 8 \o 8
            \U 16 \u 16})

(defn vfams4 [words]
  (->> words
       (group-by #(reduce (fn [score ch] (bit-or score (get vbmap ch 0))) 0 %))
       (mapv val)))

(defn vfams41 [words]
  (->> words
       (group-by #(reduce (fn [score ch] (bit-or score (get vbmap ch 0))) 0 %))
       (mapv val)))

(defn vfams42 [words]
  (->> words
       (group-by #(reduce (fn [score ch]
                            (bit-or score (get {\A 1 \a 1 \E 2 \e 2 \I 4\i 4 \O 8 \o 8 \U 16 \u
                                                16} ch 0))) 0 %))
       (mapv val)))

(defn vfams43 [words]
  (->> words
       (group-by #(reduce (fn [score ch]
                            (bit-or score (get {\A 1 \a 1 \E 2 \e 2 \I 4\i 4 \O 8 \o 8 \U 16 \u
                                                16} ch 0))) 0 %))
       (mapv val)))


;; @steffan-westcott   slow
(defn sw-vowel-families [words]
  (->> words
       (group-by #(set (filter (set "aeiou") (clojure.string/lower-case %))))
       vals
       (into [])))
