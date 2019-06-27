(ns miner.ana
  (:require [clojure.java.io :as io]))

;;; Eric Normand's Clojure challenge: anagrams
;;; https://purelyfunctional.tv/?p=31286


(defn digest [w]
  (sort (seq w)))

(defn anagram? [a b]
  (and (not= a b)
       (= (digest a) (digest b))))

(defn anagram-search [word anagram-group]
  (let [group (get anagram-group (digest word))]
    (not-empty (remove #{word} group))))

;; assume text file with one word per line
(defn load-words [filename]
  (with-open [fi (io/reader filename)]
    (binding [*in* fi]
      (doall (take-while some? (repeatedly read-line))))))

(defn anagram-group [filename]
  (group-by digest (load-words filename)))


(def default-dict "http://wiki.puzzlers.org/pub/wordlists/unixdict.txt")

(def default-anagram-group (delay (anagram-group default-dict)))

(def local-dict "/usr/share/dict/words")

(def local-anagram-group (delay (anagram-group local-dict)))
  
;; `dict` can be either a file reference or a predigested anagram group (for better performance)
(defn anagrams
  ([word]
   (anagram-search word @default-anagram-group))
  ([word dict]
   (anagram-search word (if (map? dict) dict (anagram-group dict)))))


#_
(anagrams "crate")
;; ("caret" "carte" "cater" "trace")


#_
(apply max-key count (vals @local-anagram-group))
;; ["angor" "argon" "goran" "grano" "groan" "nagor" "orang" "organ" "rogan"]


#_
(pprint (sort-by first (filter #(>= (count %) 5) (vals @default-anagram-group))))
;; (["abel" "able" "bale" "bela" "elba"]
;;  ["alger" "glare" "lager" "large" "regal"]
;;  ["angel" "angle" "galen" "glean" "lange"]
;;  ["caret" "carte" "cater" "crate" "trace"]
;;  ["elan" "lane" "lean" "lena" "neal"]
;;  ["evil" "levi" "live" "veil" "vile"])


