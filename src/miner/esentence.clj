(ns miner.esentence
  (:require [clojure.string :as str]))

;;; https://gist.github.com/ericnormand/25e53eea786708b948d0c666c790580b

;; Write a function that takes a document and a word and returns the sentences that contain
;; that word. The sentences should be returned in the order they appear in the document.
;; The search should be case insensitive.  Return nil if the word is not found.  Sentences
;; end with \., \!, or \?.

;;; SEM: what about the word being part of another word?  Does "foo" match "foobar"?  Or
;;; does the target have to be an individual word?  It's more interesting if you require
;;; exact words but I'm not sure that's the original intent.

(defn search [doc word]
  (let [word (str/lower-case word)]
    (seq (filter #(str/includes? (str/lower-case %) word)
                 (map str/triml (re-seq #"[^.!?]+[.!?]" doc))))))

(defn search2 [doc word]
  (let [word (str/lower-case word)]
    (->> doc
         (re-seq #"[^.!?]+[.!?]")
         (map str/triml)
         (filter #(str/includes? (str/lower-case %) word))
         seq)))

;; slower for trivial examples
(defn xsearch [doc word]
  (let [word (str/lower-case word)]
    (seq (sequence (filter #(str/includes? (str/lower-case %) word))
                   (sequence (map str/triml) (re-seq #"[^.!?]+[.!?]" doc))))))


(defn smoke-search [search]
  (assert (nil? (search "This is my document." "Hello")))
  (assert (= (search "This is my document. It has two sentences." "sentences")
             ["It has two sentences."]))
  (assert (= (search "Foo. Bar! Baz fooqux?" "foo")
             ["Foo." "Baz fooqux?"]))
  (assert (= (search "I like to write. Do you like to write?" "Write")
             ["I like to write." "Do you like to write?"]))
  true)

(defn smoke-search-exact [search]
  (assert (nil? (search "This is my document." "Hello")))
  (assert (= (search "This is my document. It has two sentences." "sentences")
             ["It has two sentences."]))
  (assert (= (search "Foo. Bar! Baz fooqux?" "foo")
             ["Foo."])) 
  (assert (= (search "I like to write. Do you like to write?" "Write")
             ["I like to write." "Do you like to write?"]))
  true)



;; Probably better to match only full words not fragments within other words.
(defn search-exact [doc word]
  (let [word (str/lower-case word)
        word? #(= word %)]
    (seq (filter #(some word? (re-seq #"\w+" (str/lower-case %)))
                 (map str/triml (re-seq #"[^.!?]+[.!?]" doc))))))

