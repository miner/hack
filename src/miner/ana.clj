(ns miner.ana
  (:require [clojure.java.io :as io]
            [clojure.math.combinatorics :as mc]
            [clojure.string :as str]))

;;; Eric Normand's Clojure challenge: anagrams
;;; https://purelyfunctional.tv/?p=31286

;;; revised after submission
;;; need to lowercase words, and dedupe



(defn digest [w]
  (sort (seq w)))

(defn anagram? [a b]
  (let [a (str/lower-case a)
        b (str/lower-case b)]
    (and (not= a b)
         (= (digest a) (digest b)))))

;; assume text file with one word per line
;; Note: the file could contain capitalized and lowercase versions of the same spelling so we
;; need to dedupe after lower-casing.
(defn load-words [filename]
  (into []
        (comp (map str/lower-case) (dedupe))
        (with-open [rdr (io/reader filename)] (doall (line-seq rdr)))))


;; old version
#_
(defn load-words1 [filename]
  (dedupe (map str/lower-case (with-open [rdr (io/reader filename)] (doall (line-seq rdr))))))

#_
(defn load-words0 [filename]
  (with-open [fi (io/reader filename)]
    (binding [*in* fi]
      (doall (map str/lower-case (take-while some? (repeatedly read-line)))))))

(defn anagram-group [filename]
  (group-by digest (load-words filename)))

(def default-dict "http://wiki.puzzlers.org/pub/wordlists/unixdict.txt")

(def default-anagram-group (delay (anagram-group default-dict)))

(def local-dict "/usr/share/dict/words")

(def local-anagram-group (delay (anagram-group local-dict)))
  
;; `dict` can be either a file reference or a predigested anagram group (for better performance)
(defn anagrams
  ([word]
   (anagrams word @default-anagram-group))
  ([word dict]
   (let [ana-group (if (map? dict) dict (anagram-group dict))
         word (str/lower-case word)
         grams (get ana-group (digest word))]
     (not-empty (remove #{word} grams)))))


#_
(anagrams "crate")
;; ("caret" "carte" "cater" "trace")

#_
(apply max-key count (vals @local-anagram-group))
;; ["elaps" "lapse" "lepas" "pales" "salep" "saple" "sepal" "slape" "spale" "speal"]

;; the other 10 count grouping...
;; ["angor" "argon" "goran" "grano" "groan" "nagor" "orang" "orang" "rogan" "ronga"]

#_
(pprint (sort-by first (filter #(>= (count %) 5) (vals @default-anagram-group))))
;; (["abel" "able" "bale" "bela" "elba"]
;;  ["alger" "glare" "lager" "large" "regal"]
;;  ["angel" "angle" "galen" "glean" "lange"]
;;  ["caret" "carte" "cater" "crate" "trace"]
;;  ["elan" "lane" "lean" "lena" "neal"]
;;  ["evil" "levi" "live" "veil" "vile"])





;; Harder anagrams for phrases


(def eric-original-dict
  "https://gist.githubusercontent.com/ericnormand/8c0ccc095edaa64eb8e00f861f70b02c/raw/01c33b3438bbab6bdd7e8dade55c1f5997ad8027/wordlist.txt")

;; modified by SEM to add a few words: "master" and "fun", etc to make my examples work
(def the-dict "resources/wordlist.txt")


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
        (> (count a) (count b)) -1
        (< (count a) (count b)) 1
        :else (compare a b)))
           

(defn ana-freqs [words]
  (reduce (fn [m w] (assoc m w (word-digest w))) {} words))

(defn load-freqs
  ([filename] (load-freqs 3 filename))
  ([min filename]
   (ana-freqs (remove #(< (count %) min) (load-words filename)))))

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


(def ^:dynamic *debug* false)


(defn pprint-results [results]
  (when (seq results)
    (clojure.pprint/pprint (map #(str/join " " %) (mapcat mc/permutations results)))))


;; could be faster if you kept a letter count total to prune more words

;; longest actual phrase word could be removed from initial list so we don't have to check
;; all of them at the end


;; ana is a vector of word-lists. Each word-list has a first which is the accepted word and
;; rest which has yet to be searched.

;; my rule -- don't allow any original words to be reused

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
            (do (when *debug* (println "Backtrack " (map first ana) ws))
                (recur (pop ana)
                       (add-freq remaining (get freqs (first (peek ana))))
                       (rest (peek ana))
                       results)))
          (let [word (first ws)]
            (if-let [rem1 (subtract-freq remaining (get freqs word))]
              (do (when *debug* (println "Matched" (conj ana word) (rest ws)))
                  (recur (conj ana ws)
                         rem1
                         (rest ws)
                         results))
              (do (when *debug* (println "skipping " word (map first ana) (rest ws)))
                  (recur ana remaining (rest ws) results)))))))))



(def default-freqs (load-freqs the-dict))

(defn find-anagram-phrases
  ([phrase] (find-anagram-phrases phrase default-freqs))
  ([phrase dict]
   (pprint-results (search-freqs phrase (if (map? dict) dict (load-freqs dict))))))



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

