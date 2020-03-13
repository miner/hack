(ns miner.voweldrop)

;; https://purelyfunctional.tv/issues/purelyfunctional-tv-newsletter-368-refactoring-extract-calculation-from-action/

;; Remove last vowel from words in a sentence
;;
;; Write a function that removes the last vowel from every word in a sentence. You can split
;; words by spaces. Vowels are a, e, i, o, and u.
;;
;; (remove-last-vowels "Hi, there!") ;=> "H, ther!"

;; Correction: include capitals
(defn vowel? [ch]
  (case ch
    (\a \e \i \o \u \A \E \I \O \U) true
    false))

(defn remove-last-vowels [phrase]
  (loop [i (dec (count phrase)) seen? false chv (vec (seq phrase))]
    (if (neg? i)
      (apply str chv)
      (let [c (chv i)]
        (cond (= c \space)  (recur (dec i) false chv)
              seen?         (recur (dec i) true chv)
              (vowel? c)    (recur (dec i) true (assoc chv i nil))
              :else         (recur (dec i) false chv))))))


(defn fast-remove-last-vowels [^String phrase]
  (loop [i (dec (count phrase)) seen? false sb (StringBuilder. phrase)]
    (if (neg? i)
      (.toString sb)
      (let [c (.charAt phrase i)]
        (cond (= c \space)  (recur (dec i) false sb)
              seen?         (recur (dec i) true sb)
              (vowel? c)    (recur (dec i) true (.deleteCharAt sb i))
              :else         (recur (dec i) false sb))))))







(defn rlv2 [phrase]
  (apply str (pop (reduce (fn [r i]
                            (let [seen? (peek r)
                                  c (.charAt ^String phrase  i)
                                  res (pop r)]
                              (cond (= c \space) (conj res c false)
                                    seen? (conj res c true)
                                    (vowel? c) (conj res true)
                                    :else (conj res c false))))
                          (list false)
                          (range (dec (count phrase)) -1 -1)))))









(defn smoke-rlv 
  ([] (smoke-rlv remove-last-vowels))
  ([rlv]
   (assert (= (rlv "") ""))
   (assert (= (rlv "Hi, there!") "H, ther!"))
   (assert (= (rlv "This is not a test.") "Ths s nt  tst."))
   (assert (= (rlv "Hippopotamus") "Hippopotams"))
   true))







;; other ideas but not as fast as `case`
(def ^:const set-of-vowels (set (seq "aeiou")))
(defn vw? [ch]
  (set-of-vowels ch))


(let [vvv (set (seq "aeiou"))]
(defn vow? [ch]
  (vvv ch)))


(defn slow-vow? [ch]
  (let [vvv (set (seq "aeiou"))]
    (vvv ch)))


(defn vowl? [ch]
  (#{\a \e \i \o \u} ch))


(defn slow-rlv [phrase]
  (apply str (pop (reduce (fn [r c]
                            (let [seen? (peek r)
                                  res (pop r)]
                              (cond (= c \space) (conj res c false)
                                    seen? (conj res c true)
                                    (vowel? c) (conj res true)
                                    :else (conj res c false))))
                          (list false)
                          (reverse phrase)))))

