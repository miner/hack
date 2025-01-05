(ns miner.book
  [:require [clojure.string :as str]])

;;; Original source:  https://dev.to/jorgetovar/the-clojure-paradox-41ke

;;; slight changes by SEM

(def book (slurp "https://www.gutenberg.org/cache/epub/84/pg84.txt"))

(def words (re-seq #"[\w|’]+" book))

(def common-words
  #{"a" "able" "about" "across" "after" "all" "almost" "also" "am" "among" "an"
    "and" "any" "are" "as" "at" "be" "because" "been" "but" "by" "can" "cannot"
    "could" "dear" "did" "do" "does" "either" "else" "ever" "every" "for" "from"
    "get" "got" "had" "has" "have" "he" "her" "hers" "him" "his" "how" "however"
    "i" "if" "in" "into" "is" "it" "its" "just" "least" "let" "like" "likely"
    "may" "me" "might" "most" "must" "my" "neither" "no" "nor" "not" "of" "off"
    "often" "on" "only" "or" "other" "our" "own" "rather" "said" "says" "she"
    "should" "since" "so" "some" "than" "that" "the" "their" "them" "then"
    "there" "these" "they" "this" "those" "through" "to" "too" "more" "upon"
    "us" "wants" "was" "we" "were" "what" "when" "where" "which" "while" "who"
    "whom" "why" "will" "with" "would" "yet" "you" "your" "shall" "before" "now" "one"
    "even"
    })

(defn orig-palindrome? [word]
  (= (seq word) (reverse (seq word)))
  )

;; slightly faster
(defn palindrome1? [word]
  (= word (str/reverse word)))


(defn palindrome? [^String s]
  (loop [front 0 back (dec (.length s))]
    (or (>= front back)
        (and (= (.charAt s front) (.charAt s back))
             (recur (inc front) (dec back))))))



;;; sort-by val isn't good for ties
(defn orig-frequent-words [take-n]
  (->> words
       (map str/lower-case)
       (remove common-words)
       (frequencies)
       (sort-by val)
       (take-last take-n))
  )


;;; my fixed comparison
(defn frq-compare [[k0 v0] [k1 v1]]
  (let [c (compare v0 v1)]
    (if (zero? c) (compare k0 k1) c)))

(defn orig-frequent-words-fixed [take-n]
  (->> words
       (map str/lower-case)
       (remove common-words)
       (frequencies)
       (sort frq-compare)
       (take-last take-n)))

;;; but take-last is slow!


(defn rev-frq-compare [[k0 v0] [k1 v1]]
  (let [c (compare v1 v0)]
    (if (zero? c) (compare k1 k0) c)))

;;; better to reverse sense of compare and take from head, but have to do extra rev to be
;;; compatible

(defn frequent-words [take-n]
  (->> (sequence (comp (map str/lower-case) (remove common-words)) words)
       frequencies
       (sort rev-frq-compare)
       (take take-n)
       reverse))



(defn longest-words [take-n]
  (->> words
       (map str/lower-case)
       (distinct)
       (sort-by count)
       (take-last take-n)
       (group-by count)
       )
  )

(defn longest-palindromes [take-n]
  (->> words
       (map str/lower-case)
       (distinct)
       (filter palindrome?)
       (sort-by count)
       (take-last take-n)
       )
  )

(defn original-main
  [& args]
  (println (str "Total words:" (count words)))
  (println (take 10 words))
  (println (frequent-words 10))
  (println (longest-words 10))
  (println (longest-palindromes 3))
  )

(defn book-test []
  (assert (= (count words) 78519))
  (assert (= (take 10 words) '("The"  "Project"  "Gutenberg"  "eBook"  "of"  "Frankenstein"
                               "Or"  "The"  "Modern"  "Prometheus")))
  (assert (= (frequent-words 11)
             [["saw" 94] ["towards" 94] ["being" 97] ["gutenberg" 97] ["time" 98]
              ["eyes" 104] ["first" 108] ["father" 112] ["life" 116] ["man" 132]
              ["myself" 136]]))

  (assert (= (longest-words 10)  {15  ["accomplishments"
                                       "representations"
                                       "perpendicularly"
                                       "merchantability"],
                                  16  ["indiscriminately"
                                       "impracticability"
                                       "perpendicularity"
                                       "inextinguishable"
                                       "unenforceability"],
                                  18 ["characteristically"]}))
  (assert (= (longest-palindromes 3) '("noon" "level" "sexes")))
  true)
