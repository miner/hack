(ns miner.ana
  (:require [clojure.java.io :as io]
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


