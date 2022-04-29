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
  (assert (= (letter-swaps ["aa" "bb" "ba" "ab"] "aa") [nil nil nil nil]))
  true)






;;; Made lots of wrong assumptions about problem.  Deleted some bad attemps.
;;;  Vectors of chars are slow compared to accessing chars using nth


(defn jo-letter-swap
  "When input string is equal to the target string after exactly one letter swap,
  return the sequence of letter pairs that are swapped"
  [input target]
  (when (= (count input) (count target))
    (loop [[x & xs] input
           [y & ys] target
           swap     nil]
      (cond
        (not x)        (when (set? swap) swap)
        (= x y)        (recur xs ys swap)
        (set? swap)    nil
        (nil? swap)    (recur xs ys [x y])
        (= swap [y x]) (recur xs ys (set swap))))))
(defn jo-letter-swaps [s src]
  (map (partial jo-letter-swap src) s))


(defn bn-letter-swaps [strs target]
  (let [swapped (fn swapped [fstr sstr]
                  (let [[f s] (clojure.data/diff (seq fstr) (seq sstr))
                        fv (sort (remove nil? f))
                        sv (sort (remove nil? s))]
                    (when
                      (and
                        (= (count fv) (count sv) 2)
                        (= fv sv))
                      (set sv))))]
    (map (partial swapped target) strs)))


(defn sw-letter-swap [xs ys]
  (when (= (count xs) (count ys))
    (let [[[a b] & other-swaps] (remove #(apply = %) (map vector xs ys))]
      (when (= [[b a]] other-swaps)
        #{a b}))))

(defn sw-letter-swaps [coll s]
  (mapv sw-letter-swap coll (repeat s)))



(defn pez-letter-swaps-1 [s target]
  (when-not (or
             (= s target)
             (not= (count s) (count target))
             (not= (sort s) (sort target)))
    (let [diffs (->> target
                     (map (fn [c1 c2]
                            (when-not (= c1 c2)
                              #{c1 c2}))
                          s)
                     (remove nil?))]
      (when (and (= 2 (count diffs))
                 (apply = diffs))
        (first diffs)))))

(defn pez-letter-swaps [c target]
  (mapv (fn [s] (pez-letter-swaps-1 s target)) c))
