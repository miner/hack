(ns miner.ephonetran)

;; Phone to letter translation
;; 
;; Phone keypads and rotary dials have little letters on them. Most numbers translate into
;; letters. Of course, with only 10 digits, but 26 letters, there is a problem: the
;; translation from letters to numbers is lossy. When translating from numbers to letters,
;; there is always more than one possible interpretation.
;; 
;; Write a function that takes a string of digits and returns a collection of the possible
;; strings of letters it corresponds to.
;; 
;; Here are the mappings you should use:
;; 
;; 1: no letters
;; 2: abc
;; 3: def
;; 4: ghi
;; 5: jkl
;; 6: mno
;; 7: pqrs
;; 8: tuv
;; 9: wxyz
;; 0: space
;;
;; If a character appears in the input that does not have a mapping, it will appear in the
;; output untranslated.


;;; 1: "no letters" translates as just literal 1, like any random char

;;; for convenience of reading
(def dls (reduce-kv (fn [m i s] (assoc m (first (str i)) (seq s))) {} 
                    {2 "abc"
                     3 "def"
                     4 "ghi"
                     5 "jkl"
                     6 "mno"
                     7 "pqrs"
                     8 "tuv"
                     9 "wxyz"
                     0 " "}))

;; zipmap not better



(defn digits->letters [strnum]
  (reduce (fn [res k]
            (mapcat (fn [c] (map #(str % c) res)) (dls k [k])))
          [""]
          (seq strnum)))


;; slightly faster to `case`
(defn dlm [cn]
  (case cn
    \0 '(\space)
     ;; skip 1
     \2 '(\a \b \c)
     \3 '(\d \e \f)
     \4 '(\g \h \i)
     \5 '(\j \k \l)
     \6 '(\m \n \o)
     \7 '(\p \q \r \s)
     \8 '(\t \u \v)
     \9 '(\w \x \y \z)
     (list cn)))



(defn digs [strnum]
  (reduce (fn [res k]
            (mapcat (fn [c] (map #(str % c) res)) (dlm k)))
          [""]
          (seq strnum)))


(defn xdigs [strnum]
  (transduce (map dlm)
             (fn ([res cs] (mapcat (fn [c] (map #(str % c) res)) cs))
               ([res] res)
               ([] [""]))
             (seq strnum)))

(defn sort= [a b]
  (= (sort a) (sort b)))

(defn smoke-tran [digits->letters]
  (assert (sort= (digits->letters "22") ["aa" "ab" "ac" "ba" "bb" "bc" "ca" "cb" "cc"]))
  (assert (= (digits->letters "111") ["111"]))
  (assert (= (digits->letters "1001") ["1  1"]))
  true)

;; much faster
(defn ldigs [strnum]
  (loop [res [""] ks (seq strnum)]
    (if ks
      (recur (for [c (dlm (first ks))
                   r res]
               (str r c))
             (next ks))
      res)))


;;; rdigs-slower  with vector results [elided]


;;; better ordering and a little faster
;;; BEST
(defn ydigs [strnum]
  (reduce (fn [res d]
            (for [r res
                  c (case d
                      \0 '(\space)
                      ;; skip 1
                      \2 '(\a \b \c)
                      \3 '(\d \e \f)
                      \4 '(\g \h \i)
                      \5 '(\j \k \l)
                      \6 '(\m \n \o)
                      \7 '(\p \q \r \s)
                      \8 '(\t \u \v)
                      \9 '(\w \x \y \z)
                      (list d))]                  
              (str r c)))
          [""]
          strnum))
