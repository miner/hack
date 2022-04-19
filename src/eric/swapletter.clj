(ns eric.swapletter)

;;; https://gist.github.com/ericnormand/d60a16f9e3e244aba3017e4f9af5533b

;;; Write a function that takes a sequence of strings and a target string. For each string
;;; in the sequence, determine if it is equal to the target string after exactly one letter
;;; swap. Return the sequence of letter pairs that are swapped, or nil if it doesn't exist.

;;;; Examples shows "pairs" as sets of chars.  Exact equal should return nil.
;;;; According to a commmenter, swap does NOT have to be adjacent chars

;;; not sure about   (letter-swaps ["abab"] "abba")  => [#{\a \b}]   or [nil]???
;;; I assume the latter is desired even though it seems ambiguous.

;;; Avoid working with vectors of chars as nth on String is faster.
;;; Be careful about short strings.
;;; Protect against long mismatches by add (take 3) to limit how much you collect.

(defn letter-swaps [strs target]
  (let [tcnt (count target)
        swap (fn [s]
               (when (and (not= s target) (= (count s) tcnt) (> tcnt 1))
                 (let [dv (into [] (comp (remove (fn [i] (= (nth s i) (nth target i))))
                                         (take 3))
                                (range tcnt))]
                   (when (= (count dv) 2)
                     (let [d0 (dv 0)
                           d1 (dv 1)
                           s0 (nth s d0)
                           s1 (nth s d1)]
                       (when (and (= s0 (nth target d1)) (= s1 (nth target d0)))
                         #{s0 s1}))))))]
    (mapv swap strs)))


;; slower
(defn letter-swaps6 [strs target]
  (let [tcnt (count target)
        swap (fn [s]
               (when (and (not= s target) (= (count s) tcnt) (> tcnt 1))
                 (let [rv (into [] (comp (mapcat (fn [i]
                                                   (let [si (nth s i)
                                                         ti (nth target i)]
                                                     (when (not= si ti)
                                                       (list si ti)))))
                                         (take 5))
                                (range tcnt))]
                   (when (= (count rv) 4)
                     (let [[s0 t0 s1 t1] rv]
                       (when (and (= s0 t1) (= s1 t0))
                         #{s0 s1}))))))]
    (mapv swap strs)))




(defn smoke-ls [letter-swaps]
  (assert (= (letter-swaps ["bacd" "abdc" "abcde" "abcc" "abcd" "dbca"] "abcd")
             [#{\a \b} #{\c \d} nil nil nil #{\d \a}]))
  (assert (= (letter-swaps ["b" "a" "" "bb"] "a")
             [nil nil nil nil]))
  (assert (= (letter-swaps ["ba" "aa" "bb"] "ab")
             [#{\a \b} nil nil]))
  (assert (= (letter-swaps ["abba" "bcba" "abab" "baba"] "abba")
             [nil nil #{\a \b} #{\a \b}]))
  (assert (= (letter-swaps ["aaaaaaaaaaaaaaaaaaaa"
                            "bbbbbbbbbbbbbbbbbbbb"
                            "cccccccccccccccccccc"]
                           "aaaaaaaaaaaaaaaaaaaa")
             [nil nil nil]))                           
  true)






;;; Made lots of wrong assumptions about problem.  Deleted some bad attemps.
;;;  Vectors of chars are slow compared to accessing chars using nth

;;;; other people's solutions
(defn do-find-swap [cand ref]
  (when (= (count cand) (count ref))
    (let [[swap & others] (->> (map (comp set list) cand ref)
                               (filter #(= 2 (count %))))]
      (when (= [swap] others)
        swap))))

(defn do-letter-swaps [coll ref]
  (mapv #(do-find-swap % ref) coll))
