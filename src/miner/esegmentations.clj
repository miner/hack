(ns miner.esegmentations
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

;; https://gist.github.com/ericnormand/31ba4a19b8c5b2a324ce0a1b98e4ba91

;; Your task is to write a function that takes a string without spaces and a dictionary of
;; known words and returns all possible ways it could be segmented (i.e., insert spaces)
;; into those words. If it can't be segmented, it should return an empty sequence.

;; Copied from gist, but renamed eric-segmentations

(defn append-words [a b]
  (cond
    (empty? a)
    b

    (empty? b)
    a

    :else
    (str a " " b)))

(defn segfirst [s words]
  (for [w words
        :when (clojure.string/starts-with? s w)]
    [w (.substring ^String s (.length ^String w))]))

(defn eric-segmentations [string words]
  (if (empty? string)
    [""]
    (for [[word rest]  (segfirst string words)
          segmentation (eric-segmentations rest words)]
      (append-words word segmentation))))


;; westcott solution
;; SEM added type hint
(defn append-segment [segments s dict]
  (for [len (range 1 (inc (.length ^String s)))
        :let [segment (subs s 0 len)]
        :when (contains? dict segment)]
    [(conj segments segment) (subs s len)]))

(defn segmentations* [items dict]
  (lazy-seq
    (loop [[item & more] items]
      (if-let [[segments s] item]
        (if (empty? s)
          (cons (clojure.string/join " " segments) (segmentations* more dict))
          (recur (concat (append-segment segments s dict) more)))))))

(defn w-segmentations [s dict]
  (segmentations* [[[] s]] (set dict)))


;;;;;;;;;;;;;;;;;;;;  My stuff ;;;;;;;;;;;;;;;;;


;; assume text file with one word per line
;; Note: the file could contain capitalized and lowercase versions of the same spelling so we
;; need to dedupe after lower-casing.
(defn load-words [filename]
  (into []
        (comp (map str/lower-case) (dedupe))
        (with-open [rdr (io/reader filename)] (doall (line-seq rdr)))))


;; 1010 common words
(def sem-dict "resources/sem-wordlist.txt")
(def sem-words (load-words sem-dict))
(def sem-group (group-by first sem-words))
(def sem-set (set sem-words))


;; New idea.  Walk phrase incrementally checking for word in set.  Should be faster than
;; testing all the words against phrase.

;; state [words... index-remaining]

;;; my favorite so far -- much faster if dict is already a set of words (not just a collection)
;;; only a bit faster with Java interop and hints, doesn't seem worth it

;; the "working" item on the stack is a vector of words + index of remaining string
;; slight faster than keeping "remaining" as string.
;; dict should be a set for performance, but code will convert any collection to set if
;; necessary since that was the original specification.
(defn segmentations [string dict]
  (let [dict (set dict)
        word? (fn [w] (contains? dict w))
        cnt (count string)]
    (loop [stack [[0]] res nil]
      (if-let [working (peek stack)]
        (let [words (pop working)
              index (peek working)]
          (if (= index cnt)
            (recur (pop stack) (conj res (str/join " " words)))
            (recur (into (pop stack)
                         (keep (fn [end]
                                 (let [w (subs string index end)]
                                   (when (word? w)
                                     (conj words w end)))))
                         (range cnt index -1))
                   res)))
        res))))



(defn xsegmentations [string dict]
  (let [dict (set dict)
        word? (fn [w] (contains? dict w))
        cnt (count string)]
    (loop [stack [[0]] res []]
      (if-let [working (peek stack)]
        (let [words (pop working)
              index (peek working)]
          (if (= index cnt)
            (recur (pop stack) (conj res (str/join " " words)))
            (recur (into (pop stack)
                         (comp (map #(subs string index %))
                               (filter word?)
                               (map #(conj words % (+ index (count %)))))
                         (range cnt index -1))
                   res)))
        res))))








(defn segz [string dict]
  (let [dict (set dict)
        cnt (count string)
        peek! (fn [tv] (nth tv (dec (count tv)) nil))
        into! (fn [tv xform coll] (transduce xform conj! tv coll)) ]
    (loop [state (transient [[0] []])]
      (let [results (peek! state)
            workings (persistent! (pop! state))]
      (if (zero? (count workings))
        results
        (recur (reduce (fn [res working]
                         (let [words (pop working)
                               index (peek working)
                               r (peek! res)]
                           (if (= index cnt)
                             (conj! (pop! res) (conj r (str/join " " words)))
                             (conj! (into! (pop! res)
                                         (comp (map #(subs string index %))
                                               (filter #(contains? dict %))
                                               (map #(conj words % (+ index (count %)))))
                                         (range (inc index) (inc cnt)))
                                   r))))
                       (transient [results])
                       workings)))))))



(defn segz0 [string dict]
  (let [dict (set dict)
        cnt (count string)]
    (loop [state [[0] []]]
      (let [results (peek state)
            workings (pop state)]
      (if (empty? workings)
        results
        (recur (reduce (fn [res working]
                         (let [words (pop working)
                               index (peek working)]
                           (if (= index cnt)
                             (conj (pop res) (conj (peek res) (str/join " " words)))
                             (conj (into (pop res)
                                         (comp (map #(subs string index %))
                                               (filter #(contains? dict %))
                                               (map #(conj words % (+ index (count %)))))
                                         (range (inc index) (inc cnt)))
                                   (peek res)))))
                       [results]
                       workings)))))))



;;  trying to figure out reduce approach, so far not faster
(defn segx [string dict]
  (let [dict (set dict)
        cnt (count string)]
    (loop [state {:results [] :workings [[0]]}]
      (if (empty? (:workings state))
        (:results state)
        (recur (reduce (fn [res working]
                         (let [words (pop working)
                               index (peek working)]
                           (if (= index cnt)
                             (update res :results conj (str/join " " words))
                             (update res :workings into
                                         (comp (map #(subs string index %))
                                               (filter #(contains? dict %))
                                               (map #(conj words % (+ index (count %)))))
                                         (range (inc index) (inc cnt))))))
                       (assoc state :workings [])
                       (:workings state)))))))




(defn segmentations13 [string dict]
  (let [byletter (if (map? dict) dict (group-by first dict))]
    (loop [stack [[string]] res []]
      (if-let [state (peek stack)]
        (let [words (pop state)
              remaining (peek state)
              letter (first remaining)
              candidates (byletter letter)
              matches (filter #(str/starts-with? remaining %) candidates)]
        (if (empty? matches)
          (recur (pop stack) res)
          (let [cnt (count remaining)
                finished (filter #(= remaining %) matches)
                partials (remove #(= remaining %) matches)]
            (recur (into (pop stack) (map #(conj words % (subs remaining (count %))) partials))
                   (into res (map #(conj words %) finished))))))
      (map #(str/join " " %) res)))))



(defn segmentations22 [string dict]
  (let [byletter (if (map? dict) dict (group-by first dict))]
    (loop [stack [[string]] res []]
      (if-let [state (peek stack)]
        (let [words (pop state)
              remaining (peek state)]
          (if (= remaining "")
            (recur (pop stack) (conj res words))
            (let [letter (first remaining)
                  candidates (byletter letter)
                  matches (filter #(str/starts-with? remaining %) candidates)]
              (if (empty? matches)
                (recur (pop stack) res)
                (recur (into (pop stack)
                             (map #(conj words % (subs remaining (count %))) matches))
                       res)))))
        (map #(str/join " " %) res)))))




;; my best but not so great
;; str/blanks? was slightly faster, but I like the look of = ""
(defn segmentations2 [string dict]
  (loop [stack [[string]] res []]
    (if-let [state (peek stack)]
      (let [words (pop state)
            remaining (peek state)]
        (if (= remaining "")
          (recur (pop stack) (conj res words))
          (let [matches (filter #(str/starts-with? remaining %) dict)]
            (if (empty? matches)
              (recur (pop stack) res)
              (recur (into (pop stack)
                           (map #(conj words % (subs remaining (count %))) matches))
                     res)))))
      (map #(str/join " " %) res))))

;; should have been faster than checking for empty matches but wasn't???
(defn segmentations3 [string dict]
  (loop [stack [[string]] res []]
    (if-let [state (peek stack)]
      (let [words (pop state)
            remaining (peek state)]
        (if (= remaining "")
          (recur (pop stack) (conj res words))
          (recur (into (pop stack)
                       (comp (map #(conj words % (subs remaining (count %))))
                             (filter #(str/starts-with? remaining %)))
                       dict)
                   res)))
      (map #(str/join " " %) res))))



;; state {let ind}





(defn segmentations1 [string dict]
  (loop [stack [[string]] res []]
    (if-let [state (peek stack)]
      (let [words (pop state)
            remaining (peek state)
            matches (filter #(str/starts-with? remaining %) dict)]
        (if (empty? matches)
          (recur (pop stack) res)
          (let [cnt (count remaining)
                finished (filter #(= remaining %) matches)
                partials (if (empty? finished) matches (remove #(= remaining %) matches))]
            (recur (into (pop stack) (map #(conj words % (subs remaining (count %))) partials))
                   (into res (map #(conj words %) finished))))))
      (map #(str/join " " %) res))))




;; SAVE FOR TESTING
;; state [words-already-matched ... "string-remaining"]
(defn segmentations0 [string dict]
  (loop [stack [[string]] res []]
    (if-let [state (peek stack)]
      (let [words (pop state)
            remaining (peek state)
            matches (filter #(str/starts-with? remaining %) dict)]
        (if (empty? matches)
          (recur (pop stack) res)
          (let [cnt (count remaining)
                finished (filter #(= remaining %) matches)
                partials (remove #(= remaining %) matches)]
            (recur (into (pop stack) (map #(conj words % (subs remaining (count %))) partials))
                   (into res (map #(conj words %) finished))))))
      (map #(str/join " " %) res))))





;; but slower
(defn segmentations3 [string dict]
  (loop [stack [[string]] res []]
    (if-let [state (peek stack)]
      (let [words (pop state)
            remaining (peek state)
            matches (filter #(str/starts-with? remaining %) dict)]
        (if (empty? matches)
          (recur (pop stack) res)
          (let [cnt (count remaining)
                finished (filter #(= cnt (count %)) matches)
                partials (remove #(= cnt (count %)) matches)]
            (recur (into (pop stack) (map #(conj words % (subs remaining (count %))) partials))
                   (into res (map #(conj words %) finished))))))
      (map #(str/join " " %) res))))








(def fixtures 
  [["helloworld"          ;;input string
    ["hello" "world"]      ;; input dictionary 
    ["hello world"]],       ;; expected output, in any order if more than one string

   ["abcdefghijklmnop"
    ["a" "ab" "abc" "bcd" "cdef" "ef" "def" "defg" "ghijkl" "mnop"]
    ["abc def ghijkl mnop"
     "ab cdef ghijkl mnop"
     "a bcd ef ghijkl mnop"]],

   ["penisland"
    ["pen", "penis", "land", "island"]
    ["pen island"
     "penis land"]],

   ["penisland"
    ["pen" "island"]
    ["pen island"]],

   ["expertsexchange"
    ["experts" "expert" "sex" "exchange" "change"]
    ["experts exchange"
     "expert sex change"]],

   ["expertsexchange"
    ["expert" "experts" "exchange" "ex" "change"]
    ["experts exchange"
     "experts ex change"]],

   ["expertsexchange"
    ["expert" "experts" "exchange"]
    ["experts exchange"]],

   ["hellolasdkfjlaskdjfslkj"
    ["hello" "world"]
    []]

   ["fdsfsfdsjkljf"
    ["the" "she" "a" "it"]
    []]])

(def large-fixture ["ifitistobeitisuptomeifitistobeitisuptomeifitistobeitisuptomeifitistobeitisuptomeifitistobeitisuptomeifitistobeitisuptomeifitistobeitisuptomeifitistobeitisuptomeifitistobeitisuptomeifitistobeitisuptomeifitistobeitisuptomeifitistobeitisuptomeifitistobeitisuptomeifitistobeitisuptomeifitistobeitisuptomeifitistobeitisuptomeifitistobeitisuptomeifitistobeitisuptomeifitistobeitisuptomeifitistobeitisuptomeifitistobeitisuptomeifitistobeitisuptomeifitistobeitisuptomeifitistobeitisuptomeifitistobeitisuptomeifitistobeitisuptomeifitistobeitisuptomeifitistobeitisuptomeifitistobeitisuptomeifitistobeitisuptomeifitistobeitisuptomeifitistobeitisuptomeifitistobeitisuptomeifitistobeitisuptomeifitistobeitisuptomeifitistobeitisuptomeifitistobeitisuptomeifitistobeitisuptomeifitistobeitisuptome", 
["about", "be", "hell", "if", "is", "it", "me", "other", "outer", "people", "the", "to", "up", "where"],
["if it is to be it is up to me if it is to be it is up to me if it is to be it is up to me if it is to be it is up to me if it is to be it is up to me if it is to be it is up to me if it is to be it is up to me if it is to be it is up to me if it is to be it is up to me if it is to be it is up to me if it is to be it is up to me if it is to be it is up to me if it is to be it is up to me if it is to be it is up to me if it is to be it is up to me if it is to be it is up to me if it is to be it is up to me if it is to be it is up to me if it is to be it is up to me if it is to be it is up to me if it is to be it is up to me if it is to be it is up to me if it is to be it is up to me if it is to be it is up to me if it is to be it is up to me if it is to be it is up to me if it is to be it is up to me if it is to be it is up to me if it is to be it is up to me if it is to be it is up to me if it is to be it is up to me if it is to be it is up to me if it is to be it is up to me if it is to be it is up to me if it is to be it is up to me if it is to be it is up to me if it is to be it is up to me if it is to be it is up to me if it is to be it is up to me"]
                    ])


(defn set= [a b]
  (= (set a) (set b)))

(defn smoke-fixtures [f]
  (doseq [[input dict result] fixtures]
    (assert (set= (f input dict) result) input))
  (let [[input dict result] large-fixture]
    (assert (set= (f input dict) result) input))
  true)



(def www ["this" "is" "only" "a" "test" "not" "really"])

(defn smoke-seg [f]
  (assert (= (f "thisisonlyatest" www) '("this is only a test")))
  (assert (empty? (f "thiswasenytatest" www)))
  (assert (= (f "thisisonlyatest" sem-words) '("this is only a test")))
  (assert (empty? (f "thiswasenytatest" sem-words)))
  true)



(defn smoke-group [f]
  (assert (= (f "thisisonlyatest" sem-group) '("this is only a test")))
  (assert (empty? (f "thiswasenytatest" sem-group)))
  true)


(defn smoke-set [f]
  (assert (= (f "thisisonlyatest" sem-set) '("this is only a test")))
  (assert (empty? (f "thiswasenytatest" sem-set)))
  true)
