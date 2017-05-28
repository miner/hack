(ns miner.delimited
  (:require [clojure.string :as str]))

;; http://www.coconut-palm-software.com/bliki/doku.php?id=blog%3Aenterprise_clojure_and_specs&rev=1495743440&utm_source=dlvr.it&utm_medium=twitter


;; ORIGINAL CODE

(^:private def delimiters [\'])
(^:private def delimiter-set (set delimiters))
 
(defn merge-strings
  "Given a vector of strings, merge strings beginning/ending with quotes into
  a single string and return a vector of standalone words and quoted strings.
  Nested / unbalanced quotes will return undefined results."
  [[result delimiter merging] next]
 
  (let [start (first (seq next))
        end   (last (seq next))]
    (cond
      (and ((set delimiters) start)
           ((set delimiters) end))   [(conj result next) nil ""]
      ((set delimiters) start)       [result start next]
      ((set delimiters) end)         [(conj result (str merging " " next)) nil ""]
      (nil? delimiter)               [(conj result next) nil ""]
      :else                          [result delimiter (str merging " " next)])))
 
 
(defn delimited-words
  "Split a string into words, respecting single or double quoted substrings.
  Nested quotes are not supported.  Unbalanced quotes will return undefined
  results."
  [s]
  (let [words (str/split s #"\s")
        delimited-word-machine (reduce merge-strings [[] nil ""] words)
        merged-strings (first delimited-word-machine)
        remainder (last delimited-word-machine)
        delimiter (second delimited-word-machine)]
    (if (empty? remainder)
      merged-strings
      (conj merged-strings (str remainder delimiter)))))

;; SEM: article has more about adding specs, but for now I want to look at the code
;; END -- ORIGINAL CODE


(comment
  ;; I think the original code is buggy with spaces just inside the single quote
  ;; but let's say you can't have those ' spaces '.
  (def yyy "This is a ' Hello World ' so called 'test' internal")
  (delimited-words yyy)
;=> ["This" "is" "a" "'" "Hello" "World" "'" "so" "called" "'test'" "internal"]
)


;; Some of my proposed solutions for timing tests.  Some might be wrong, so try smoke-test
;; first.  In particular, there's an issue that the first string my be "" (empty string)
;; because of the way str/split works when the source starts with a delimiter.  Also
;; sequential delimited strings and trailing spaces.  Also, I think leading and trailing
;; spaces within quotes should be preserved, but original didn't do that so I skip my test
;; only for the original.


(defn smoke-test
  ([] (smoke-test delimited-words))
  ([delimited]
   (and (= (delimited "java com.hello.Hello 'Hello world'")
           ["java" "com.hello.Hello" "'Hello world'"])
        (= (delimited "one ") ["one"])
        (= (delimited "  ") [])
        (= (delimited "'This' 'That'") ["'This'" "'That'"])
        (= (delimited "'This' or 'That'") ["'This'" "or" "'That'"])
        (or (= delimited delimited-words)
            ;; delimited-words doesn't handle internal spaces in quotes
            (and (= (delimited "") [])
                 (= (delimited " one") ["one"])
                 (= (delimited "' internal spaces '") ["' internal spaces '"]))))))

   

;; fastest so far
;; allows leading and trailing spaces in quoted words
(defn my-delim [s]
  (persistent!
   (reduce-kv (fn [r i x]
                (cond
                 (odd? i) (conj! r (str "'" x "'"))
                 (str/blank? x) r
                 :else (reduce conj! r (str/split (str/trim x) #"\s"))))
              (transient [])
              (str/split s #"'"))))



;;;;;; LOTS OF JUNKs

(defn WORKS-my-delim [s]
  (persistent!
   (reduce-kv (fn [r i x]
                (cond
                 (odd? i) (conj! r (str "'" x "'"))
                 (str/blank? x) r
                 :else (reduce conj! r (str/split x #"\s"))))
              (transient [])
              (str/split s #"\s*'\s*"))))

;; fails on sequential quoted "'one' 'two'", need to drop blanks
(defn BAD-my-delim [s]
  (persistent!
   (reduce-kv (fn [r i x]
                (cond
                 (odd? i) (conj! r (str "'" x "'"))
                 (and (zero? i) (str/blank? x)) r
                 :else (reduce conj! r (str/split x #"\s"))))
              (transient [])
              (str/split s #"\s*'\s*"))))




;; too slow, and ugly
(defn SLOW-ldelim [s]
  (loop [cs (seq s) w [] words []]
    (if (nil? cs)
      (if (zero? (count w))
        words
        (conj words (apply str w)))
      (case (first cs)
        \' (let [qw (take-while #(not= \' %) (rest cs))]
             (recur (next (drop (inc (count qw)) cs))
                    []
                    (conj words (str \' (apply str qw) \'))))
        (\ ) (recur (next cs)
                    []
                    (if (zero? (count w))
                      words
                      (conj words (apply str w))))
        (recur (next cs)
               (conj w (first cs))
               words)))))

;; even slower
(defn ldelim [s]
  (loop [cs (seq s) w [] words []]
    (if (nil? cs)
      (map #(apply str %)
           (if (zero? (count w))
             words
             (conj words w)))
      (case (first cs)
        \' (let [qw (into (into [\'] (take-while #(not= \' %) (rest cs))) [\'])]
             (recur (next (drop-while #(not= \' %) (rest cs)))
                    []
                    (conj words qw)))
        (\ ) (recur (next cs)
                    []
                    (if (zero? (count w))
                      words
                      (conj words w)))
        (recur (next cs)
               (conj w (first cs))
               words)))))



(defn SLOW-delim [s]
  (reduce (fn [r x]
          (if (coll? x)
            (into r x)
            (conj r x)))
          []
  (map-indexed (fn [i x] (if (odd? i)
                           (str "'" x "'")
                           (str/split x #"\s")))
               (str/split s #"\s*'\s*"))))
  

(defn delimX [s]
  (apply concat
          (map-indexed (fn [i x] (if (odd? i)
                           [(str "'" x "'")]
                           (str/split x #"\s")))
                       (str/split s #"\s*'\s*"))))



(defn tdelim [s]
  (into []
   (comp (map-indexed (fn [i x]
                        (cond
                         (odd? i) [(str "'" x "'")]
                         (str/blank? x) []
                         :else (str/split (str/trim x) #"\s"))))
         cat)
   (str/split s #"'")))

;; slower with partition
(defn tdelim2 [s]
  (into []
   (comp (partition-all 2)
         (mapcat (fn [[x q]]
                   (cond (nil? q) (str/split (str/trim x) #"\s")
                         (str/blank? x) [(str "'" q "'")]
                         :else (conj (str/split (str/trim x) #"\s") (str "'" q "'"))))))
   (str/split s #"'")))



;; Be careful some of these don't handle sequential quotes or internal quoted spaces

(defn tdelim3x [s]
  (into [] (comp (map-indexed
                    (fn [i x] (if (odd? i)
                                [(str "'" x "'")]
                                (when-not (zero? (.length ^String x)) (str/split x #"\s")))))
                   cat)
        (str/split s #"\s*'\s*")))


(defn tdelim1x [s]
  (into [] (comp (map-indexed
                    (fn [i x] (if (odd? i)
                                [(str "'" x "'")]
                                (when-not (str/blank? x) (str/split x #"\s")))))
                   cat)
        (str/split s #"\s*'\s*")))

(defn tdelim2x [s]
  (into [] (comp (map-indexed
                  (fn [i x] (cond
                             (and (zero? i) (str/blank? x)) nil
                             (odd? i) [(str "'" x "'")]
                             :else (str/split x #"\s"))))
                   cat)
        (str/split s #"\s*'\s*")))




(defn rdelim [s]
  (reduce-kv (fn [r i x] (cond
                        (and (zero? i) (str/blank? x)) r
                        (odd? i) (conj r (str "'" x "'"))
                        :else (into r (str/split x #"\s"))))
             []
             (str/split s #"\s*'\s*")))


