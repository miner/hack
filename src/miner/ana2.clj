(ns miner.ana2
  (:require [clojure.java.io :as io]
            [clojure.math.combinatorics :as mc]
            [clojure.string :as str]))

;; https://purelyfunctional.tv/issues/purelyfunctional-tv-newsletter-333-tool-rebel-readline/
;; Harder anagrams for phrases

;; My solution uses frequency maps of the characters of the dictionary words and compares
;; them to the frequency map of the source phrase.  As a first pass, I collect the words
;; that could individually match part of the phrase.  Then I test the longest words first
;; and search for a combination that works.  As a word matches, I save the whole remaining
;; word list on the "ana" stack.  If a sequence fails, I backtrack and start again with rest
;; of that word-list segment.  If a sequence succeeds, the anagram is taken from the first
;; of each word-list on the stack.  When pretty-printing the final results, I expand all the
;; permutations of the matching words.

;; Some executive decisions:
;;  1.  Don't allow anagram phrases to contain any words from the original phrase.
;;  2.  Skip dictionary words of fewer than three characters.
;;  3.  Add a few words to the source dictionary to make my test examples work.

(def eric-dict
  "https://gist.githubusercontent.com/ericnormand/8c0ccc095edaa64eb8e00f861f70b02c/raw/wordlist.txt")

;; A few extra words to make my test examples work
(def extra-words ["astronomer" "cinder" "fun" "master" "mint" "miser" "Roman" "severe"])

;; modified by SEM to add the extra words for my test examples
#_
(def sem-dict "resources/sem-wordlist.txt")


(defn word-digest [word]
  (-> word
      str/lower-case
      frequencies))

(defn phrase-digest [phrase]
  (-> phrase
      str/lower-case
      (str/replace #"[^a-z]" "")
      frequencies))

(defn phrase-words [phrase]
  (-> phrase
      str/lower-case
      (str/replace #"[^a-z]" " ")
      str/trim
      (str/split #" +")))

(defn ana-phrase? [phrase1 phrase2]
  (and (= (phrase-digest phrase1) (phrase-digest phrase2))
       (not= (sort (phrase-words phrase1)) (sort (phrase-words phrase2)))))

;; longest words first, = length by alphabetical order
(defn compare-word-length-alpha [^String a ^String b]
  (cond (and (nil? a) (nil? b)) 0
        (nil? a) -1
        (nil? b) 1
        (> (.length a) (.length b)) -1
        (< (.length a) (.length b)) 1
        :else (compare a b)))

;; assume text file with one word per line
;; Note: the file could contain capitalized and lowercase versions of the same spelling so we
;; need to dedupe after lower-casing.
(defn load-words [filename]
  (into []
        (comp (map str/lower-case) (dedupe))
        (with-open [rdr (io/reader filename)] (doall (line-seq rdr)))))

;; by default consider only words of 3 or more characters
(defn load-freqs
  ([filename] (load-freqs 3 [] filename))
  ([min extras filename]
   (reduce (fn [m w] (assoc m w (word-digest w)))
           {}
           (concat extras
                   (remove #(< (count %) min) (load-words filename))))))


;; subtract character counts from frequency map
;; dissoc character when count would be zero
;; returns nil for failure

(defn subtract-freq [working freq]
  (reduce-kv (fn [res ch cnt]
               (let [old (get res ch)]
                 (cond (nil? old) (reduced nil)
                       (= cnt old) (dissoc res ch)
                       (< cnt old) (update res ch - cnt)
                       :else (reduced nil))))
             working
             freq))

(defn add-freq [working freq]
  (merge-with + working freq))

(defn pprint-results [results]
  (when (seq results)
    (clojure.pprint/pprint (map #(str/join " " %) (mapcat mc/permutations results)))))


;; could be faster if you kept a letter count total to prune more words

;; ana is a vector of word-lists. Each word-list has a first which is the accepted word and
;; rest which has yet to be searched.


(defn search-freqs [phrase dict-freqs]
  (let [pdig (phrase-digest phrase)
        freqs (reduce dissoc dict-freqs (phrase-words phrase))
        xwords (sort compare-word-length-alpha
                     (filter #(subtract-freq pdig (get freqs %)) (keys freqs)))]
    (loop [ana [] remaining pdig ws xwords results []]
      (if (empty? remaining)
        ;; found an anagram, backtrack for more
        (recur (pop ana)
               (add-freq remaining (get freqs (first (peek ana))))
               (rest (peek ana))
               (conj results (map first ana)))
        (if (empty? ws)
          (if (empty? ana)
            ;; finished
            results
            ;; backtrack
            (recur (pop ana)
                   (add-freq remaining (get freqs (first (peek ana))))
                   (rest (peek ana))
                   results))
            (if-let [rem1 (subtract-freq remaining (get freqs (first ws)))]
              (recur (conj ana ws) rem1 (rest ws) results)
              (recur ana remaining (rest ws) results)))))))

(def default-freqs (load-freqs 3 extra-words eric-dict))

(defn anagram-lists
  ([phrase] (anagram-lists phrase default-freqs))
  ([phrase dict]
   (search-freqs phrase (if (map? dict) dict (load-freqs dict)))))

(defn pprint-anagram-phrases
  ([phrase] (pprint-anagram-phrases phrase default-freqs))
  ([phrase dict]  (pprint-results (anagram-lists phrase dict))))


(defn smoke-test-phrases []
  (assert (ana-phrase? "the classroom" "school master"))
  (assert (not (ana-phrase? "master school" "school master")))
  (assert (not (ana-phrase? "school master" "School  Master")))
  (assert (ana-phrase? "Astronomer" "Moon starer"))
  (assert (ana-phrase? "The Eyes" "They see"))
  (assert (ana-phrase? "Steve Miner" "event miser"))
  (assert (ana-phrase? "Steve Miner" "severe mint"))
  (assert (ana-phrase? "Eric Normand" "Roman cinder"))
  true)


(defn my-phrase-test []
  (let [results (set (anagram-lists "Steve Miner"))]
    (assert (not (contains? results ["even" "steven"])))
    (assert (contains? results '("event" "miser")))
    (assert (contains? results '("severe" "mint"))))
  true)

