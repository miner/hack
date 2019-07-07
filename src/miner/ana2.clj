(ns miner.ana2
  (:require [clojure.java.io :as io]
            [clojure.math.combinatorics :as mc]
            [clojure.string :as str]))

;; https://purelyfunctional.tv/issues/purelyfunctional-tv-newsletter-333-tool-rebel-readline/
;; Harder anagrams for phrases

(def eric-dict
  "https://gist.githubusercontent.com/ericnormand/8c0ccc095edaa64eb8e00f861f70b02c/raw/wordlist.txt")

;; modified by SEM to add a few words to make my examples work
(def the-dict "resources/sem-wordlist.txt")

(def extra-words ["astronomer" "cinder" "fun" "master" "mint" "miser" "Roman" "severe"])

(defn word-digest [word]
  (-> word
      str/lower-case
      frequencies))

(defn phrase-digest [phrase]
  (-> phrase
      (str/replace #"[^A-Za-z]" "")
      str/lower-case
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

;; Executive decision: by default only consider words of 3 or more characters
(defn load-freqs
  ([filename] (load-freqs 3 filename))
  ([min filename]
   (reduce (fn [m w] (assoc m w (word-digest w)))
           {}
           (remove #(< (count %) min) (load-words filename)))))

;; returns nil for failure
(defn subtract-freq [working freq]
  (reduce-kv (fn [res ch cnt]
               (if (>= (get res ch 0) cnt)
                 (update res ch - cnt)
                 (reduced nil)))
             working
             freq))

(defn add-freq [working freq]
  (merge-with + working freq))

(defn empty-working? [m]
  (every? zero? (vals m)))


(defn pprint-results [results]
  (when (seq results)
    (clojure.pprint/pprint (map #(str/join " " %) (mapcat mc/permutations results)))))


;; could be faster if you kept a letter count total to prune more words

;; ana is a vector of word-lists. Each word-list has a first which is the accepted word and
;; rest which has yet to be searched.

;; my rule -- don't allow any original words to be appear in anagram results

(defn search-freqs [phrase dict-freqs]
  (let [pdig (phrase-digest phrase)
        freqs (reduce dissoc dict-freqs (phrase-words phrase))
        xwords (sort compare-word-length-alpha
                     (filter #(subtract-freq pdig (get freqs %)) (keys freqs)))]
    (loop [ana [] remaining pdig ws xwords results []]
      (if (empty-working? remaining)
        ;; found one, backtrack for more
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
          (let [word (first ws)]
            (if-let [rem1 (subtract-freq remaining (get freqs word))]
              (recur (conj ana ws)
                     rem1
                     (rest ws)
                     results)
              (recur ana remaining (rest ws) results))))))))



(def default-freqs (load-freqs the-dict))

(defn anagram-phrases
  ([phrase] (anagram-phrases phrase default-freqs))
  ([phrase dict]
   (search-freqs phrase (if (map? dict) dict (load-freqs dict)))))

(defn pprint-anagram-phrases
  ([phrase] (pprint-anagram-phrases phrase default-freqs))
  ([phrase dict]  (pprint-results (anagram-phrases phrase dict))))



;;; could short circuit if there's a letter with no words
;;; maybe no "x" or "z"
;;; or count of letters
;;; should look for words with least likely letters first
;;;
;;; or filter all words first to see if they could be in, then try to search that list which
;;; should be much smaller.  Avoids reconsidering dead words a million times.




(defn smoke-phrase []
  (assert (ana-phrase? "the classroom" "school master"))
  (assert (not (ana-phrase? "master school" "school master")))
  (assert (not (ana-phrase? "school master" "School  Master")))
  (assert (ana-phrase? "Astronomer" "Moon starer"))
  (assert (ana-phrase? "The Eyes" "They see"))
  (assert (ana-phrase? "Steve Miner" "event miser"))
  (assert (ana-phrase? "Steve Miner" "severe mint"))
  (assert (ana-phrase? "Eric Normand" "Roman cinder"))
  true)

(defn local-phrase-test []
  (let [results (set (search-freqs "Steve Miner" default-freqs))]
    (assert (not (contains? results ["even" "steven"])))
    (assert (contains? results ["event" "miser"]))
    (assert (contains? results ["severe" "mint"])))
  true)

