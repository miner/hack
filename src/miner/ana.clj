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

;; modified by SEM to add a few words: "master" and "fun"
(def eric-local-dict "resources/wordlist.txt")


(defn word-digest [word]
  (-> word
      str/lower-case
      frequencies))

(defn phrase-digest [phrase]
  (-> phrase
      (str/replace #"[^A-Za-z]" "")
      str/lower-case
      frequencies))

(defn phrase-words-sorted [phrase]
  (-> phrase
      str/lower-case
      (str/replace #"[^a-z]" " ")
      str/trim
      (str/split #" +")
      sort))


(defn ana-phrase? [phrase1 phrase2]
  (and (not= (phrase-words-sorted phrase1) (phrase-words-sorted phrase2))
       (= (phrase-digest phrase1) (phrase-digest phrase2))))

;; longest words first, = length by alphabetical order
(defn compare-word-length-alpha [^String a ^String b]
  (cond (and (nil? a) (nil? b)) 0
        (nil? a) -1
        (nil? b) 1
        (> (count a) (count b)) -1
        (< (count a) (count b)) 1
        :else (compare a b)))
           

(defn ana-freqs [words]
  (reduce (fn [m w] (assoc m w (word-digest w))) (sorted-map-by compare-word-length-alpha) words))

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


;; unify state of ana and working into one stack of maps

;; shouldn't match original!


;;; got to control backtracking correctly -- not from the top to repeat everything
;;; should search for all anagrams!
;;; but not return original -- spaces significant in final

(def ^:dynamic *debug* false)


(defn pprint-results [results]
  (clojure.pprint/pprint (map #(str/join " " %) (mapcat mc/permutations results))))

(defn search-freqs [phrase freqs]
  (let [pdig (phrase-digest phrase)
        phwords (phrase-words-sorted phrase)
        xfreqs (reduce-kv (fn [r k v]
                            (if (subtract-freq pdig (get freqs k))
                              r
                              (dissoc r k)))
                          freqs
                          freqs)]
    (println "Freqs count" (count freqs) "  xfreqs" (count xfreqs))

  (loop [ana [] remaining pdig fqs xfreqs results []]
    (if (empty-working? remaining)
      ;; found one, backtrack for more
      (recur (pop ana)
             (add-freq remaining (get xfreqs (peek ana)))
             (reduce dissoc xfreqs (concat ana (map key (subseq xfreqs < (peek ana)))))
             (conj results ana))
      (if (empty? fqs)
        (if (empty? ana)
          ;; finished
          (not-empty (remove #(= (phrase-words-sorted %) phwords) results))
          ;; backtrack
          (do (when *debug* (println "Backtrack " ana (keys fqs)))
              (recur (pop ana)
                     (add-freq remaining (get xfreqs (peek ana)))
                     (reduce dissoc xfreqs (concat ana (map key (subseq xfreqs < (peek
                                                                                  ana)))))
                     results)))
        (let [word (key (first fqs))]
          (if-let [rem1 (subtract-freq remaining (get xfreqs word))]
            (do (when *debug* (println "Matched" (conj ana word) (keys (dissoc fqs word))))
                (recur (conj ana word)
                       rem1
                       (dissoc fqs word)
                       results))
            (do (when *debug* (println "skipping " word ana (keys (dissoc fqs word))))
                (recur ana remaining (dissoc fqs word) results)))))))))


(defn find-anagram-phrases [phrase dict]
  (pprint-results (search-freqs phrase (if (map? dict) dict (load-freqs dict)))))



;;; could short circuit if there's a letter with no words
;;; maybe no "x" or "z"
;;; or count of letters
;;; should look for words with least likely letters first
;;;
;;; or filter all words first to see if they could be in, then try to search that list which
;;; should be much smaller.  Avoids reconsidering dead words a million times.



;;; testing only
(def bbb (ana-freqs ["foo" "bar" "baz" "boing"]))

(def eee (load-freqs eric-local-dict))

(defn smoke-anap []
  (assert (ana-phrase? "the classroom" "school master"))
  (assert (not (ana-phrase? "master school" "school master")))
  (assert (not (ana-phrase? "school master" "School  Master")))
  (assert (ana-phrase? "Astronomer" "Moon starer"))
  (assert (ana-phrase? "The Eyes" "They see"))
  (assert (ana-phrase? "Steve Miner" "event miser"))
  (assert (ana-phrase? "Steve Miner" "severe mint"))
  (assert (ana-phrase? "Eric Normand" "modern cairn"))
  (assert (ana-phrase? "Eric Normand" "Roman cinder"))
  true)
